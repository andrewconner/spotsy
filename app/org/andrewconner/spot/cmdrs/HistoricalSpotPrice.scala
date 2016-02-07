package org.andrewconner.spot.cmdrs

import java.io.File

import cats.data.Xor
import com.amazonaws.regions.Regions
import com.amazonaws.services.ec2.model.{ DescribeSpotPriceHistoryRequest, DescribeSpotPriceHistoryResult }
import org.andrewconner.spot.cmdrs.HistoricalSpotPrice._
import org.andrewconner.spot.cmdrs.instance.EC2Platform
import org.andrewconner.spot.core._
import org.andrewconner.spot.core.time.Clock
import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try
import scalaz.Nondeterminism
import scalaz.concurrent.Task

object HistoricalSpotPrice {
  import Regions._
  type TimePrice = (DateTime, Price)
  val minutesPerSample = 60 / 4 // 4 samples per hour, 15 min samples
  val descriptions = Seq("Linux/UNIX")
  val regions = Seq(US_WEST_1, US_WEST_2, US_EAST_1 /*, EU_WEST_1, EU_CENTRAL_1, AP_SOUTHEAST_1, AP_SOUTHEAST_2, AP_NORTHEAST_1, AP_NORTHEAST_2*/ )
}
class HistoricalSpotPrice(awsClient: AWSClient, clock: Clock)(implicit ec: ExecutionContext) {

  private val outDir = new File("./history/")

  private val loadMemo = Memu.expiringMemo[Unit, Future[Map[InstanceKind, PriceHistory]]](minutesPerSample.minutes, clock.now) { _ =>
    forceLoadAll().runFuture().map { _.collect { case p if p.history.nonEmpty => p.kind -> p }.toMap }
  }
  def load(kind: InstanceKind): Future[Option[PriceHistory]] = loadMemo(()).map(_.get(kind))
  def loadAll(): Future[Seq[PriceHistory]] = loadMemo(()).map(_.values.toSeq)

  private val startBackdate = new DateTime("2016-03-01T18:00:00.000-00:00")
  def forceLoadAll(): Task[List[PriceHistory]] = existingHistoriesT.flatMap { existingHistories =>
    val groupedHistories = existingHistories.groupBy(h => (Regions.fromName(h.kind.region), h.kind.description))
    val histories = for {
      desc <- descriptions
      region <- regions
    } yield {
      val existing = groupedHistories.getOrElse((region, desc), List.empty)

      val minExistingDateOpt = existing.flatMap { h => h.history.headOption.map(_._1) }.minOpt
      val maxExistingDateOpt = existing.flatMap { h => h.history.lastOption.map(_._1) }.maxOpt

      val fetches = (minExistingDateOpt, maxExistingDateOpt) match {
        case (Some(minD), Some(maxD)) =>
          val before = if (minD.minusMinutes(minutesPerSample).isAfter(startBackdate)) {
            Seq(fetchHistoryT(startBackdate, minD.plusMinutes(minutesPerSample), region, desc))
          } else Seq()
          val after = if (maxD.plusMinutes(minutesPerSample).isBefore(clock.now)) {
            Seq(fetchHistoryT(maxD.minusMinutes(minutesPerSample), clock.now, region, desc))
          } else Seq()
          before ++ after
        case (_, _) => Seq(fetchHistoryT(startBackdate, clock.now, region, desc))
      }

      println(s"Processing $region, $desc. ${existing.length} existing histories... Performing ${fetches.length} fetches.")

      Nondeterminism[Task].gatherUnordered(fetches).flatMap { fs =>
        val together = fs.flatten
        if (together.nonEmpty) {
          println(s"Finished fetching for $region, $desc. ${together.length} new histories fetched.")
          mergeHistories(existing, together)
        } else {
          Task.now(existing)
        }
      }
    }

    Nondeterminism[Task].gatherUnordered(histories).map(_.flatten)
  }

  // Existing known histories
  private val existingHistoriesT = Nondeterminism[Task].gatherUnordered {
    outDir.listFiles().filter(!_.getName.startsWith(".")).map { file =>
      Task(PriceHistory.fromFile(file).leftMap(f => println(s"Couldn't load ${file.getName}, ${f.getMessage}")))
    }
  }.map { r => r.collect { case Xor.Right(h) => h } }

  private def persistHistoryToFile(hs: PriceHistory) = Task(PriceHistory.saveToFile(outDir, hs))

  // Assumes existing doesn't have duplicate kinds
  private def mergeHistories(existing: List[PriceHistory], newHistories: List[PriceHistory]): Task[List[PriceHistory]] = {
    val buffer = mutable.HashMap[InstanceKind, (List[TimePrice], Boolean)]().withDefaultValue((List.empty, false))

    existing.foreach { h =>
      buffer += ((h.kind, (h.history, false)))
    }

    newHistories.foreach { newHistory =>
      val existing = buffer(newHistory.kind)._1
      val merged = mergeSortedTimeSeries(existing, newHistory.history).toList
      //println(s" - Adding for ${newHistory.kind}, ${existing.length} + ${newHistory.history.length} = ${merged.length}")
      buffer += ((newHistory.kind, (merged, true)))
    }

    val persisted = buffer.map {
      case (kind, (ts, wasChanged)) =>
        val hist = PriceHistory(kind, ts)
        if (wasChanged && ts.nonEmpty) {
          persistHistoryToFile(hist).map { _ => hist }
        } else {
          Task.now(hist)
        }
    }

    Nondeterminism[Task].gatherUnordered(persisted.toSeq)
  }

  // Fetch history
  private def fetchHistoryT(startTime: DateTime, endTime: DateTime, region: Regions, descriptions: String): Task[List[PriceHistory]] = {
    val requestTemplate = new DescribeSpotPriceHistoryRequest()
      .withProductDescriptions(descriptions)
      .withStartTime(startTime.toDate)
      .withEndTime(endTime.toDate)

    println(s"Fetching for ${region.getName} $descriptions from $startTime to $endTime...")

    fetchCompleteHistory(region)(requestTemplate).map { results =>
      results.map(c => PriceHistory(c._1, c._2.toList)).toList
    }
  }

  // Fetching from Amazon
  private def fetchCompleteHistory(region: Regions)(request: DescribeSpotPriceHistoryRequest): Task[Map[InstanceKind, Seq[TimePrice]]] = {
    def fetch(token: String): Task[List[Map[InstanceKind, Seq[TimePrice]]]] = {
      awsClient.fetchSpotPrices(region)(request, token).map { result =>
        val read = readHistoryResult(result)
        val nextToken = Option(result.getNextToken)
        (read, nextToken)
      }.recover {
        case ex: Throwable =>
          println(s"AWS Client error! ${region.getName} ${request.toString} ${ex.getMessage}")
          Task.now((Map.empty[InstanceKind, List[TimePrice]], None))
      }.flatMap {
        case ((read, nextToken)) =>
          val _latest = read.flatMap(_._2.headOption.map(_._1)).maxOpt.map(_.toString).getOrElse("(none)")
          println(s"  ------ Fetching in ${region.getName} ${token.take(10)}... got ${read.size} kinds, ${read.map(_._2.length).sum} price changes up to ${_latest}.")
          if (read.nonEmpty && nextToken.exists(_.nonEmpty)) {
            fetch(nextToken.getOrElse("")).map { read :: _ }
          } else {
            Task.now(read :: Nil)
          }
      }
    }
    def reduce(results: List[Map[InstanceKind, Seq[TimePrice]]]) = {
      results.foldLeft(mutable.HashMap.empty[InstanceKind, Seq[TimePrice]]) {
        case (acc, result) =>
          result.map {
            case (kind, prices) =>
              acc.get(kind) match {
                case Some(existing) =>
                  acc += ((kind, mergeSortedTimeSeries(existing, prices)))
                case None =>
                  acc += ((kind, prices))
              }
          }
          acc
      }.toMap
    }

    fetch("").map(reduce)
  }

  private def readHistoryResult(result: DescribeSpotPriceHistoryResult): Map[InstanceKind, List[TimePrice]] = {
    result.getSpotPriceHistory.groupBy { h =>
      InstanceKind(h.getInstanceType, h.getProductDescription, h.getAvailabilityZone)
    }.map {
      case (kind, full) =>
        kind -> full.flatMap { p =>
          Price.applyOpt(p.getSpotPrice).map { price =>
            new DateTime(p.getTimestamp) -> price
          }
        }.toList.sortBy(_._1)
    }
  }

  // Assumes the timeseries is sorted already and non-empty, earliest first
  // Return datetimes are front-biased. So, time 15:30 covers one period *before* that time.
  // A price at 15:22 will change it.
  private def bucketTS(timeseries: Seq[TimePrice]) = {

    def ceilToBucket(time: DateTime): DateTime = {
      val min = time.minuteOfHour.get
      val offset = ((min / minutesPerSample + 1) * minutesPerSample) % 60
      time.withMinuteOfHour(offset).withSecondOfMinute(0)
    }

    val ts = mutable.ListBuffer[TimePrice]()

    // currPrice and currTime are with respect to the output bucketed quantitized ts
    // os = original series

    var osIdx = 0
    var (osTime, osPrice) = timeseries(osIdx)
    var currPrice = osPrice
    var currTime = ceilToBucket(timeseries(osIdx)._1)
    var lastPrice = osPrice

    while (osIdx < timeseries.length) {
      val nextTime = currTime.plusMinutes(minutesPerSample)
      var firstPriceOfBlock = true
      while (osTime.isBefore(nextTime) && osIdx < timeseries.length) {
        osTime = timeseries(osIdx)._1
        osPrice = timeseries(osIdx)._2

        currPrice = if (firstPriceOfBlock) {
          osPrice
        } else {
          lastPrice = osPrice
          Price(Math.max(currPrice.value, osPrice.value))
        }
        firstPriceOfBlock = false
        osIdx += 1
      }
      if (lastPrice != currPrice) {
        ts.append((currTime, currPrice))
      }
      currTime = nextTime
    }
    ts.toList
  }

  private def mergeSortedTimeSeries(x: Seq[TimePrice], y: Seq[TimePrice]): Seq[TimePrice] = {
    assertIsSorted(x); assertIsSorted(y)

    val acc = new mutable.ListBuffer[TimePrice]()
    @tailrec
    def rec(xs: Seq[TimePrice], ys: Seq[TimePrice]): Unit = {
      (xs, ys) match {
        case (ex, ey) if ex.isEmpty && ey.isEmpty => ()
        case (_, ey) if ey.isEmpty =>
          acc ++= xs; ()
        case (ex, _) if ex.isEmpty =>
          acc ++= ys; ()
        case (xh +: xt, yh +: yt) =>
          if (xh._1 isBefore yh._1) { acc += xh; rec(xt, ys) }
          else if (xh._1.isEqual(yh._1)) { acc += ((xh._1, Price(Math.max(xh._2.value, yh._2.value)))); rec(xt, yt) }
          else { acc += yh; rec(xs, yt) }
      }
    }
    rec(x, y)

    assertIsSorted(acc)

    acc.toSeq
  }

  private def assertIsSorted(ls: Seq[TimePrice]) = {
    assert(ls.sortBy(_._1) == ls, "List is not sorted!")
  }
}

// todo: Typesafe.. omg, InstanceType is missing some
// instanceType: c1.xlarge
// description: Linux/UNIX
// zone: us-west-1a
case class InstanceKind(instanceType: String, description: String, zone: String) {
  def region = zone.dropRight(1)
  def platform = {
    description match {
      case "Linux/UNIX" => EC2Platform.linux
      case other => throw new Exception(s"Unknown platform for $description")
    }
  }
}

case class Price(value: Double) extends AnyVal
object Price {
  def applyOpt(p: String) = Try(Price(p.toDouble)).toOption
}

