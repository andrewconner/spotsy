package org.andrewconner.spot

import java.util.concurrent.{ CancellationException, Future => JFuture, TimeUnit }

import _root_.io.netty.util.{ HashedWheelTimer, Timeout, TimerTask }
import org.joda.time.{ DateTimeZone, DateTime }
import play.api.libs.iteratee.Execution.Implicits

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.concurrent.{ Future, Promise }
import scala.util.{ Failure, Success, Try }
import scalaz.Memo
import scalaz.concurrent.Task

package object core extends Implicits {

  def stack(count: Int = 5, filter: String = "") = {
    new Exception().getStackTrace.drop(2).filter(s => (s.getClassName + s.getMethodName).contains(filter)).take(count).map(l => l.getClassName + "." + l.getMethodName + "(" + l.getFileName + ":" + l.getLineNumber + ")").mkString("\n")
  }

  // todo move to Implicits

  // This timer is important to pay attention to. Since we're polling for future completion, if this number is too high,
  // you will experience slow future completion. i.e., if you're using this in a performance-sensitive context, you may
  // want to set it lower than the default. However, most IO tasks are not highly performance sensitive, so by default,
  // we're using 100 ms. Make sure you reuse the same `timer` application-wide, because it will take a dedicated thread.
  private val pollIntervalMs = 100L
  private val timer = new HashedWheelTimer(pollIntervalMs, TimeUnit.MILLISECONDS)

  implicit class JFutureHelpers[T](javaFuture: JFuture[T]) {
    def toScala: Future[T] = {
      val promise = Promise[T]()

      def checkCompletion(): Unit = {
        if (javaFuture.isCancelled) {
          promise.failure(new CancellationException())
        } else if (javaFuture.isDone) {
          promise.complete(Try(javaFuture.get))
        } else {
          scheduleTimeout()
        }
        ()
      }

      def scheduleTimeout(): Unit = {
        timer.newTimeout(new TimerTask {
          override def run(timeout: Timeout): Unit = checkCompletion()
        }, pollIntervalMs, TimeUnit.MILLISECONDS)
        ()
      }

      checkCompletion()
      promise.future
    }
  }

  object Memu {
    // work:
    def expiringMemo[K, V](expiresAfter: Duration, clock: => DateTime): Memo[K, V] = { // todo make threadsafe
      val a = new collection.mutable.HashMap[K, (DateTime, V)]()
      Memo.memo[K, V] { f =>
        k =>
          a.get(k) match {
            case Some((date, v)) if date.plus(expiresAfter.toMillis).isAfter(clock) => v
            case _ => val n = f(k); a.update(k, (clock, n)); n
          }
      }
    }

    def expiringLoadingMemo[K, T, V](expiresAfter: Duration, clock: => DateTime): Memo[K, T => V] = { // todo make threadsafe
      val a = new collection.mutable.HashMap[K, (DateTime, V)]()
      Memo.memo[K, T => V] { f =>
        k =>
          a.get(k) match {
            case Some((date, v)) if date.plus(expiresAfter.toMillis).isAfter(clock) =>
              t: T => v
              case _ =>
              t: T => val n = f(k)(t); a.update(k, (clock, n)); n
          }
      }
    }
  }

}

final class AnyExtensionOps[A](val x: A) extends AnyVal {
  // forward pipe operator, analogous to the Unix pipe.
  // Uses symbolic method name for order-of-operation reasons (and ubiquity of |)
  // expr |> updateA |> updateB |> save
  @inline def |>[B](f: A => B): B = f(x)

  // Kestrel Combinator, tee operator
  // expr tap println
  @inline def tap(f: A => Unit): A = {
    f(x)
    x
  }
}

final class FuncExtensionOpts[A](f: => A) {
  @inline def recover(g: PartialFunction[Throwable, A]): A = {
    try f catch {
      case ex: Throwable if g.isDefinedAt(ex) => g(ex)
      case ex: Throwable => throw ex
    }
  }
}

final class TryExtensionOps[A](val x: scala.util.Try[A]) extends AnyVal {
  def fold[B](f: A => B, g: Throwable => B): B = x match {
    case scala.util.Success(t) => f(t)
    case scala.util.Failure(t) => g(t)
  }
}

final class FutureExtensionOps[A](x: => Future[A]) {
  import scalaz.Scalaz._

  def asTask: Task[A] = {
    Task.async { register =>
      x.onComplete {
        case Success(v) =>
          register(v.right)
        case Failure(ex) => register(ex.left)
      }(Implicits.trampoline)
    }
  }
}

final class TaskExtensionOps[A](x: => Task[A]) {
  import scalaz.{ -\/, \/- }
  val p: Promise[A] = Promise()
  def runFuture(): Future[A] = {
    x.unsafePerformAsync {
      case -\/(ex) =>
        p.failure(ex); ()
      case \/-(r) => p.success(r); ()
    }
    p.future
  }
}

final class IterableExtensionOps[A, Repr](xs: IterableLike[A, Repr]) {
  def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
    val builder = cbf(xs.repr)
    val i = xs.iterator
    var set = Set[B]()
    while (i.hasNext) {
      val o = i.next()
      val b = f(o)
      if (!set(b)) {
        set += b
        builder += o
      }
    }
    builder.result()
  }
}

final class TraversableOnceExtensionOps[A](xs: TraversableOnce[A]) {
  def maxOpt(implicit cmp: Ordering[A]): Option[A] = if (xs.isEmpty) None else Option(xs.max)
  def minOpt(implicit cmp: Ordering[A]): Option[A] = if (xs.isEmpty) None else Option(xs.min)
  def maxByOpt[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
    if (xs.isEmpty) None else Option(xs.maxBy(f))
  }
  def minByOpt[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
    if (xs.isEmpty) None else Option(xs.minBy(f))
  }
}

final class TraversableExtensionOps[A](xs: Traversable[A]) {
  def countBy[B](fn: A => B): Map[B, Int] = xs.groupBy(fn).mapValues(_.toSeq.length)
  def countAll: Map[A, Int] = countBy(identity)
}

final class SlickDatabaseExtensionOps(db: => slick.backend.DatabaseComponent#DatabaseDef) {
  import slick.dbio.{ DBIOAction, NoStream }

  import scalaz.Scalaz._
  def runTask[R](a: DBIOAction[R, NoStream, Nothing]): Task[R] = {
    Task.async { register =>
      db.run(a).onComplete {
        case Success(v) => register(v.right)
        case Failure(ex) => register(ex.left)
      }(Implicits.trampoline)
    }

  }
}

trait DateTimeImplicits {
  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
}

trait Implicits extends DateTimeImplicits {
  implicit def anyExtensionOps[A](x: A): AnyExtensionOps[A] = new AnyExtensionOps[A](x)
  implicit def tryExtensionOps[A](x: scala.util.Try[A]): TryExtensionOps[A] = new TryExtensionOps[A](x)
  implicit def funcExtensionOps[A](x: => A): FuncExtensionOpts[A] = new FuncExtensionOpts[A](x)
  implicit def futureExtensionOps[A](x: => Future[A]): FutureExtensionOps[A] = new FutureExtensionOps[A](x)
  implicit def iterableExtensionOps[A, Repr](xs: IterableLike[A, Repr]): IterableExtensionOps[A, Repr] = new IterableExtensionOps(xs)
  implicit def traversableOnceExtensionOps[A](xs: TraversableOnce[A]): TraversableOnceExtensionOps[A] = new TraversableOnceExtensionOps(xs)
  implicit def traversableExtensionOps[A](xs: Traversable[A]): TraversableExtensionOps[A] = new TraversableExtensionOps(xs)
  implicit def taskExtensionOps[A](x: => Task[A]): TaskExtensionOps[A] = new TaskExtensionOps(x)
  implicit def slickDatabaseExtensionOps(db: => slick.backend.DatabaseComponent#DatabaseDef): SlickDatabaseExtensionOps = new SlickDatabaseExtensionOps(db)

}

