package org.andrewconner.spot.playapp

import java.util.concurrent.{ Callable, TimeUnit }

import akka.actor.ActorSystem
import akka.stream.Materializer
import org.andrewconner.spot.core.{ TaskActionBuilder, TaskAction }
import org.joda.time.DateTime
import play.api.mvc._
import org.feijoas.mango.common.cache._
import play.api.http.Status

import scala.collection.mutable
import scala.concurrent.{ Promise, Future }
import scala.concurrent.duration._
import play.api.libs.iteratee.Execution.Implicits.trampoline

import scalaz.concurrent.Task

trait Tarpit {
  val maxRequests: Int
  val graceThreshhold: Int
  val timePeriod: Duration
  val maxDelay: Duration

  private val maxCacheSize = 1024L
  private val bucketsPerPeriod = 4

  import Tarpit._

  def addAndCheck(clientId: String): TarpitDelay = {
    calcDelay(updateClientAndGetRequestCount(clientId))
  }

  type SecondOfDay = Int
  type ClientHitRecord = mutable.Buffer[(SecondOfDay, Int)]
  private lazy val requestCountCache: LoadingCache[String, ClientHitRecord] =
    CacheBuilder.newBuilder()
      .expireAfterAccess(timePeriod.length, timePeriod.unit)
      .concurrencyLevel(1)
      .maximumSize(maxCacheSize)
      .build { _: String => mutable.Buffer.empty[(SecondOfDay, Int)] }

  private def updateClientAndGetRequestCount(clientId: String) = {
    val cache: ClientHitRecord = requestCountCache(clientId)
    addEvent(cache, new DateTime())
    cache.map(_._2).sum
  }

  private def timeToKey(time: DateTime) = {
    ((time.getMillisOfDay * bucketsPerPeriod) / timePeriod.toMillis).toInt
  }

  private def addEvent(cache: ClientHitRecord, time: DateTime) = cache.synchronized {
    val key = timeToKey(time)

    cache.lastOption match {
      case Some((lk, lv)) if lk == key =>
        cache(cache.length - 1) = (key, lv + 1)
      case _ =>
        cache += ((key, 1))
    }

    if (cache.length > bucketsPerPeriod) {
      cache.remove(0, cache.length - bucketsPerPeriod)
    }
  }

  private def sigmoid(t: Double) = 1.0 / (1 + Math.pow(Math.E, -t))
  private def calcDelay(hitCount: Int): TarpitDelay = {
    if (hitCount < graceThreshhold) {
      NoDelay
    } else if (hitCount > maxRequests) {
      println(s" - blocking, $hitCount requests")
      Block
    } else {
      val asymptoticMax = 6.0

      val delayMs = sigmoid((hitCount.toDouble / maxRequests.toDouble) * asymptoticMax * 2 - asymptoticMax) * maxDelay.toMillis
      if (delayMs < 10.0) {
        NoDelay
      } else {
        println(s" - delaying ${delayMs.toInt}ms")
        DelayMs(delayMs.toInt)
      }
    }
  }
}

object Tarpit {
  sealed trait TarpitDelay
  case object Block extends TarpitDelay
  case object NoDelay extends TarpitDelay
  case class DelayMs(value: Int) extends TarpitDelay
}

class TarpitFilter(actorSystem: ActorSystem)(implicit val mat: Materializer) extends Filter {
  val tarpit = new Tarpit {
    val timePeriod = 1.minutes
    val maxDelay = 6.seconds
    val maxRequests: Int = 100
    val graceThreshhold = maxRequests / 5
  }

  def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    import Tarpit._

    tarpit.addAndCheck(requestHeader.remoteAddress) match {
      case NoDelay =>
        nextFilter(requestHeader)
      case Block =>
        Future.successful(Results.Status(Status.BAD_GATEWAY))
      case DelayMs(ms: Int) =>
        val promise = Promise[Unit]()
        actorSystem.scheduler.scheduleOnce(ms.milliseconds) { promise.success((): Unit); () }
        promise.future.flatMap { _ => nextFilter(requestHeader) }(trampoline)
    }
  }
}

// TarpitAction(10, 1.minute, 3.seconds)
// TarpitAction(10, 1.minute)
// TarpitAction(10)

trait BaseActions {
  def actorSystem: ActorSystem // for scheduling of the tarpit

  def TarpitAction(maxRequests: Int, within: Duration, maxDelay: Duration) = new TarpitActionBuilder(actorSystem, maxRequests, within, maxDelay)
}

trait BaseController extends Controller with BaseActions {

}

class TarpitActionBuilder(actorSystem: ActorSystem, maxRequests: Int, timePeriod: Duration, maxDelay: Duration) extends TaskActionBuilder[Request] {
  private val tarpit: Tarpit = {
    val _maxRequests = maxRequests; val _timePeriod = timePeriod; val _maxDelay = maxDelay
    new Tarpit {
      val maxRequests = _maxRequests
      val timePeriod = _timePeriod
      val maxDelay = _maxDelay
      val graceThreshhold = _maxRequests / 5
    }
  }

  def invokeBlockT[A](request: Request[A], block: (Request[A]) => Task[Result]) = {
    block(request)
  }
}
