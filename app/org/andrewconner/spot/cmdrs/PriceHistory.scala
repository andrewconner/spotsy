package org.andrewconner.spot.cmdrs

import java.io.{ FileNotFoundException, PrintWriter, File }

import cats.data.Xor
import org.joda.time.DateTime

import scala.collection.mutable
import scala.io.Source

case class PriceHistory(kind: InstanceKind, history: List[(DateTime, Price)]) {
  import HistoricalSpotPrice.TimePrice
  def continuousBucketedHistory(now: DateTime): Stream[TimePrice] = {
    val mt = Stream.empty[TimePrice]
    import HistoricalSpotPrice._

    def ceilToBucket(time: DateTime): DateTime = {
      val min = time.minuteOfHour.get
      val offset = ((min / minutesPerSample + 1) * minutesPerSample) % 60
      time.withMinuteOfHour(offset).withSecondOfMinute(0)
    }

    def stream(bucketTime: DateTime, bucketPrice: Price, currentPrice: Price, hs: List[TimePrice]): Stream[TimePrice] = {
      val nextBucketTime = bucketTime.plusMinutes(HistoricalSpotPrice.minutesPerSample)
      hs match {
        case Nil if bucketTime.isBefore(now) =>
          (bucketTime, bucketPrice) #:: stream(nextBucketTime, bucketPrice, bucketPrice, Nil)
        case Nil => mt
        case hh :: ht if !hh._1.isAfter(bucketTime) => // Same bucket. Update bucket price, no new price emitted
          stream(bucketTime, Price(Math.max(hh._2.value, bucketPrice.value)), hh._2, ht)
        case hh :: ht => // Next price is different bucket, but more periods exist in between
          (bucketTime, bucketPrice) #:: stream(nextBucketTime, currentPrice, currentPrice, hs)
      }
    }

    history match {
      case Nil => mt
      case hh :: ht =>
        val startTime = ceilToBucket(hh._1)
        stream(startTime, hh._2, hh._2, ht)
    }
  }
}

object PriceHistory {

  def toFilename(kind: InstanceKind) = {
    s"${kind.zone}-${kind.instanceType}-${kind.description.replace('/', '+')}"
  }
  def saveToFile(outDir: File, history: PriceHistory): File = {
    val safe = toFilename(history.kind).replace('/', '+').replace(' ', '_')
    val out = new StringBuilder()
    /*
version:1
<zone>
<instanceType>
<description>
<date>  # for each
<price> # record
     */
    out ++= "version:1"
    out ++= "\n"
    out ++= history.kind.instanceType
    out ++= "\n"
    out ++= history.kind.zone
    out ++= "\n"
    out ++= history.kind.description
    var prevPrice = 0.0
    history.history.foreach {
      case ((date, price)) =>
        if (price.value != prevPrice) {
          out ++= "\n"
          out ++= date.toString // todo choose DateTime format?
          out ++= "\n"
          out ++= price.value.toString
          prevPrice = price.value
        }
    }

    val file = new File(outDir.getPath + "/" + safe)
    val pw = new PrintWriter(file)
    pw.write(out.result())
    pw.close()
    file
  }

  def fromFile(file: File): Throwable Xor PriceHistory = Xor.catchNonFatal {

    Xor.catchOnly[FileNotFoundException](Source.fromFile(file).getLines()).flatMap { lines =>
      val version = lines.next()
      if (version == "version:1") {
        val instanceType = lines.next()
        val zone = lines.next()
        val description = lines.next()
        val kind = InstanceKind(instanceType, description, zone)

        val builder = new mutable.ListBuffer[(DateTime, Price)]()

        if (lines.hasNext) {
          var currDate = new DateTime(lines.next())
          var currPrice = Price.applyOpt(lines.next()).get
          builder += ((currDate, currPrice))

          while (lines.hasNext) {
            val nextDate = new DateTime(lines.next())
            val nextPrice = Price.applyOpt(lines.next()).get

            currDate = currDate.plusMinutes(HistoricalSpotPrice.minutesPerSample)

            // Fill in any missing rows
            while (currDate.isBefore(nextDate)) {
              currDate = currDate.plusMinutes(HistoricalSpotPrice.minutesPerSample)
            }
            currPrice = nextPrice
            currDate = nextDate
            builder += ((currDate, currPrice))
          }
        }

        Xor.right(PriceHistory(kind, builder.toList))
      } else {
        Xor.left(new RuntimeException("invalid history version"))
      }
    }
  }.flatMap(s => s)
}

