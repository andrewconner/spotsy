package org.andrewconner.spot.cmdrs

import org.andrewconner.spot.cmdrs.client.{ InstanceStats, SpotPriceStats }
import org.andrewconner.spot.cmdrs.instance._
import org.andrewconner.spot.core._
import org.andrewconner.spot.core.time.Clock
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scalaz.concurrent.Task

class InstanceHistoryBuilder(
    historicalSpotPrice: HistoricalSpotPrice,
    ec2InstanceDetailsFetcher: EC2InstanceDetailsFetcher,
    clock: Clock,
    implicit val ec: ExecutionContext
) {

  val statsMemo = Memu.expiringLoadingMemo[(InstanceKind, Double), (EC2Instance, PriceHistory, DateTime, DateTime, DateTime), Option[InstanceStats]](1.minute, clock.now) {
    case (kind, threshold) => in =>
      val (instance, priceHistory, now, yesterday, lastWeek) = in
      EC2InstanceChoice.get(instance)(EC2Region(priceHistory.kind.region), priceHistory.kind.platform).flatMap { choice =>
        val allPrices = priceHistory.continuousBucketedHistory(now).toList
        val recentPrices = allPrices.dropWhile(_._1.isBefore(lastWeek))

        if (recentPrices.nonEmpty) {
          val all = spotStats(allPrices.map(_._2), choice, threshold)
          val recent = spotStats(recentPrices.map(_._2), choice, threshold)

          val inst = choice.instance
          Some(InstanceStats(
            inst.name,
            inst.prettyName,
            inst.memory,
            inst.ecu,
            inst.cores,
            inst.storage,
            inst.arch,
            inst.networkPerformance,
            inst.enhancedNetworking,
            inst.vpcOnly,
            inst.linuxVirtualizationType,
            inst.ebsOptimized,
            priceHistory.kind.zone,
            choice.region,
            choice.platform,
            choice.price,
            threshold,
            recent,
            all,
            priceSince(yesterday, now, priceHistory),
            priceSince(lastWeek, now, priceHistory),
            priceHistory.history.length,
            allPrices.length
          ))
        } else {
          None
        }
      }
  }

  def stats(filters: Seq[InstanceFilter], threshold: Double): Task[Seq[InstanceStats]] = {
    val now = clock.now
    val yesterday = now.minusHours(24)
    val lastWeek = now.minusWeeks(1)

    for {
      history <- historicalSpotPrice.loadAll().asTask
      instances <- validInstances(filters)
    } yield {
      val stats = for {
        priceHistory <- history
        instance <- instances.get(priceHistory.kind.instanceType)
        choice <- EC2InstanceChoice.get(instance)(EC2Region(priceHistory.kind.region), priceHistory.kind.platform)
      } yield {
        statsMemo((priceHistory.kind, threshold))((instance, priceHistory, now, yesterday, lastWeek))
      }

      stats.flatten
    }
  }

  val instancesMemo = Memu.expiringMemo[Unit, Future[Map[String, EC2Instance]]](1.day, clock.now) { _ =>
    ec2InstanceDetailsFetcher.value.runFuture()
  }
  private def validInstances(filters: Seq[InstanceFilter]): Task[Map[String, EC2Instance]] = {
    instancesMemo(()).asTask.map { instances =>
      instances
        .filter(i => filters.forall(f => f.run(i._2))) // Meets passed in requirements
    }
  }

  private def spotStats(p: List[Price], choice: EC2InstanceChoice, threshold: Double) = {
    val prices = p.map(_.value)
    val sortedPrices = prices.sorted
    val avg = prices.sum / prices.length
    val max = sortedPrices.lastOption.getOrElse(0.0)
    val ci = {
      val ciPrice = sortedPrices(Math.min(Math.ceil(sortedPrices.length.toDouble * threshold).toInt, sortedPrices.length - 1))
      sortedPrices.find(_ > ciPrice).getOrElse(ciPrice)
    }
    val costPerECU = ci / choice.instance.ecu
    val discount = ci / choice.price.ondemand
    val costPerGbRam = ci / choice.instance.memory

    SpotPriceStats(avg = avg, max = max, perECU = costPerECU, perGbRam = costPerGbRam, discount = discount, price = ci)
  }

  private def priceSince(since: DateTime, now: DateTime, priceHistory: PriceHistory): Option[Double] = {
    val rawIdx = priceHistory.history.indexWhere(_._1.isAfter(since))
    val idx = if (rawIdx >= 0) rawIdx else priceHistory.history.length - 1
    if (idx == 0) {
      None
    } else {
      val series = ((since, priceHistory.history(idx)._2) :: priceHistory.history.drop(idx)) :+ ((now, Price(0.0)))
      val price = series.map(r => r._1.getMillis -> r._2.value).sliding(2).map {
        case Seq((d1, p1), (d2, _)) =>
          val tdiff = (d2 - d1).toDouble / 1000.0 / 60.0 / 60.0
          tdiff * p1
      }.sum
      Some(price)
    }
  }

}
