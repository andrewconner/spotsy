package org.andrewconner.spot.controllers

import org.andrewconner.spot.cmdrs._
import org.andrewconner.spot.cmdrs.instance._
import org.andrewconner.spot.core._
import org.andrewconner.spot.core.time.Clock
import org.joda.time.Period
import play.api.{ Play, Logger }
import play.api.libs.iteratee._
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import scalaz.concurrent.Task
import scala.concurrent.duration._

class SpotController(instanceHistoryBuilder: InstanceHistoryBuilder, historicalSpotPrice: HistoricalSpotPrice, ec2InstanceDetailsFetcher: EC2InstanceDetailsFetcher, clock: Clock)(implicit ec: ExecutionContext) extends Controller {
  lazy val history = {
    val f = historicalSpotPrice.loadAll()
    f.asTask
  }

  def graph(search: Option[String]) = Action {
    Ok(views.html.priceGraph(search))
  }

  def csvHistory(search: Option[String]) = TaskAction.task {
    val terms = search.getOrElse("").split("\\+ ").map(_.trim).filter(_.nonEmpty).toSet
    history.map { results =>
      val header = "name,date,price"
      val csv = results.flatMap { result =>
        val name = Seq(result.kind.instanceType, result.kind.zone, result.kind.description)
        if (terms.forall(t => name.exists(n => n.contains(t)))) {
          val nameStr = name.mkString("-")
          result.continuousBucketedHistory(clock.now).takeRight(100).map { h =>
            s"$nameStr,${h._1},${h._2.value}"
          }
        } else {
          Seq()
        }
      }.mkString("\n")
      Ok(header + "\n" + csv)
    }
  }

  def test(filter: ResultFilter) = TaskAction.task {
    instanceHistoryBuilder.stats(filter.instanceFilters, filter.ci.getOrElse(0.99)).map { statss =>
      val table = statss.flatMap { stats =>
        val filtered = Seq(
          filter.discount.exists(_ > 1 - stats.weeklySpotStats.discount)
        ).forall(r => r)

        if (filtered) None else Some(stats)
      }.toList.sortBy { r =>
        filter.sort.getOrElse("") match {
          case "ecu" => r.ecu
          case "mem" => r.memory
          case "$day" => r.pricePastDay.getOrElse(Double.MaxValue)
          case "$week" => r.pricePastWeek.getOrElse(Double.MaxValue)
          case "demand" => r.prices.ondemand
          case "avg" => r.weeklySpotStats.avg
          case "max" => r.weeklySpotStats.max
          case "threshold" => r.weeklySpotStats.price
          case "$ecu" => r.weeklySpotStats.perECU
          case "$ram" => r.weeklySpotStats.perGbRam
          case "discount" | _ => r.weeklySpotStats.discount
        }
      }.map { stats =>
        def fmt(d: Double) = f"$d%1.5f".take(5)

        val d1 = Seq(stats.name, stats.zone, stats.ecu.toString, stats.memory.toString, stats.priceSamples.toString, stats.periods.toString)
        val d2 = Seq(stats.pricePastDay.getOrElse(0.0), stats.pricePastWeek.getOrElse(0.0), stats.prices.ondemand, stats.weeklySpotStats.avg, stats.weeklySpotStats.max, stats.weeklySpotStats.price, stats.weeklySpotStats.perECU, stats.weeklySpotStats.perGbRam, 1 - stats.weeklySpotStats.discount).map(fmt)
        (d1 ++ d2).mkString("\t")
      }.mkString("\n")
      val out = "name\t\tzone\t\tecu\tmem\tcnt\tperiods\t$day\t$week\tdemand\tavg\tmax\t95ile\t$/ecu\t$/ram\t% off\n\n" ++ table
      Ok(out)
    }
  }

}

case class ResultFilter(
    mem: ResultFilter.Rng,
    ecu: ResultFilter.Rng,
    cpu: ResultFilter.Rng,
    storage: ResultFilter.Rng,
    ssd: Option[Boolean],
    arch: Option[Set[EC2Arch]],
    networkPerformance: Option[Set[EC2NetworkPerformance]],
    enhancedNetworking: Option[Boolean],
    vpcOnly: Option[Boolean],
    linuxVirtualizationTypes: Option[Set[EC2VirtualizationType]],
    ebsOptimized: Option[Boolean],

    ci: Option[Double],
    discount: Option[Double],
    // todo: zone, region, $day, $week, avg$, etc
    sort: Option[String]
) {
  def instanceFilters = {
    import org.andrewconner.spot.cmdrs.instance.{ InstanceFilter => F }
    Seq(
      F.memory(mem.min, mem.max),
      F.ecu(ecu.min, ecu.max),
      F.cpu(cpu.min, cpu.max),
      F.storageSize(storage.min, storage.max),
      F.storageSsd(ssd),
      F.arch(arch),
      F.networkPerformance(networkPerformance),
      F.enhancedNetworking(enhancedNetworking),
      F.vpcOnly(vpcOnly),
      F.linuxVirtualizationTypes(linuxVirtualizationTypes),
      F.ebsOptimized(ebsOptimized)
    )
  }
}

object ResultFilter {
  case class Rng(min: Option[Double], max: Option[Double])

  implicit def queryStringBinder(implicit strBinder: QueryStringBindable[String], doubBinder: QueryStringBindable[Double]) = new QueryStringBindable[ResultFilter] {
    private def subBind[T](key: String, subkey: String, params: Map[String, Seq[String]])(implicit b: QueryStringBindable[T]): Option[T] = {
      b.bind(s"$key.$subkey", params).flatMap(_.right.toOption)
    }

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, ResultFilter]] = {
      def bnd[T](s: String)(implicit b: QueryStringBindable[T]) = subBind[T](key, s, params)
      Some(Right(ResultFilter(
        mem = Rng(bnd[Double]("mem.min"), bnd[Double]("mem.max")),
        ecu = Rng(bnd[Double]("ecu.min"), bnd[Double]("ecu.max")),
        cpu = Rng(bnd[Double]("cpu.min"), bnd[Double]("cpu.max")),
        storage = Rng(bnd[Double]("storage.min"), bnd[Double]("storage.max")),
        ssd = bnd[Boolean]("ssd"),
        arch = bnd[String]("arch").map(_.split(",").map(EC2Arch(_)).toSet),
        networkPerformance = bnd[String]("networkPerformance").map(_.split(",").map(EC2NetworkPerformance(_)).toSet),
        enhancedNetworking = bnd[Boolean]("enhancedNetworking"),
        vpcOnly = bnd[Boolean]("vpcOnly"),
        linuxVirtualizationTypes = bnd[String]("linuxVirtualizationTypes").map(_.split(",").map(EC2VirtualizationType(_)).toSet),
        ebsOptimized = bnd[Boolean]("ebsOptimized"),
        ci = bnd[Double]("ci"),
        discount = bnd[Double]("discount"),
        sort = bnd[String]("sort")
      )))
    }
    override def unbind(key: String, filter: ResultFilter): String = {
      ""
    }
  }
}
