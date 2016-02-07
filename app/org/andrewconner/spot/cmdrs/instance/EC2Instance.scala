package org.andrewconner.spot.cmdrs.instance

import cats.data.Xor
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

// eii is short for ec2instances.info, where we scrape the data from

case class EC2ReservedPricing(yr1NoUpfront: Double, yr1PartialUpfront: Double, y1AllUpfront: Double, yr3PartialUpfront: Double, yr3AllUpfront: Double)
object EC2ReservedPricing {

  def isDouble(s: String) = {
    Xor.catchOnly[NumberFormatException](s.toDouble) match {
      case Xor.Left(l) => false
      case Xor.Right(n) => true
    }
  }

  val eiiReads: Reads[EC2ReservedPricing] = (
    (JsPath \ "yrTerm1.noUpfront").read[String].filter(isDouble(_)).map(_.toDouble) and
    (JsPath \ "yrTerm1.partialUpfront").read[String].filter(isDouble(_)).map(_.toDouble) and
    (JsPath \ "yrTerm1.allUpfront").read[String].filter(isDouble(_)).map(_.toDouble) and
    (JsPath \ "yrTerm3.partialUpfront").read[String].filter(isDouble(_)).map(_.toDouble) and
    (JsPath \ "yrTerm3.allUpfront").read[String].filter(isDouble(_)).map(_.toDouble)
  )(EC2ReservedPricing.apply _)
}

case class EC2Pricing(ondemand: Double, reserved: Option[EC2ReservedPricing])
object EC2Pricing {
  import EC2ReservedPricing.isDouble
  val eiiReads: Reads[EC2Pricing] = (
    (JsPath \ "ondemand").read[String].filter(isDouble(_)).map(_.toDouble) and
    (JsPath \ "reserved").readNullable[EC2ReservedPricing](EC2ReservedPricing.eiiReads).orElse(Reads.pure(None))
  )(EC2Pricing.apply _)
}

case class EC2Platform(name: String)
object EC2Platform {
  val eiiReads: Reads[EC2Platform] = Reads.of[String].map(EC2Platform(_))

  val linux = EC2Platform("linux")
  val mswin = EC2Platform("mswin")
}

case class EC2Region(name: String)
object EC2Region {
  val eiiReads: Reads[EC2Region] = Reads.of[String].map(EC2Region(_))
}

case class EC2Storage(ssd: Boolean, devices: Int, size: Double)
case object EC2Storage {
  val eiiReads: Reads[EC2Storage] = (
    (JsPath \ "ssd").read[Boolean] and
    (JsPath \ "devices").read[Int] and
    (JsPath \ "size").read[Double]
  )(EC2Storage.apply _)
}

case class EC2Arch(name: String)
case object EC2Arch {
  val eiiReads: Reads[EC2Arch] = Reads.of[String].map(EC2Arch(_))
}

case class EC2NetworkPerformance(name: String)
case object EC2NetworkPerformance {
  val eiiReads: Reads[EC2NetworkPerformance] = Reads.of[String].map(EC2NetworkPerformance(_))
}

case class EC2VirtualizationType(name: String)
case object EC2VirtualizationType {
  val eiiReads: Reads[EC2VirtualizationType] = Reads.of[String].map(EC2VirtualizationType(_))
}

case class EC2Instance(
  name: String,
  prettyName: String,
  memory: Double,
  ecu: Double,
  cores: Double,
  storage: Option[EC2Storage],
  arch: Seq[EC2Arch],
  networkPerformance: EC2NetworkPerformance,
  enhancedNetworking: Boolean,
  vpcOnly: Boolean,
  linuxVirtualizationType: Seq[EC2VirtualizationType],
  ebsOptimized: Boolean,
  prices: Map[EC2Region, Map[EC2Platform, EC2Pricing]]
)
object EC2Instance {
  val eiiReads: Reads[EC2Instance] = {
    implicit val stora = EC2Storage.eiiReads
    implicit val arch = EC2Arch.eiiReads
    implicit val netPerf = EC2NetworkPerformance.eiiReads
    implicit val region = EC2Region.eiiReads
    implicit val virt = EC2VirtualizationType.eiiReads
    implicit def regionPricing = new Reads[Map[EC2Region, Map[EC2Platform, EC2Pricing]]] {
      def reads(j: JsValue): JsResult[Map[EC2Region, Map[EC2Platform, EC2Pricing]]] = {
        val p = j.as[JsObject].fields.collect {
          case (reg, JsObject(pp)) =>
            EC2Region(reg) -> pp.flatMap {
              case (pl, pr) =>
                EC2Pricing.eiiReads.reads(pr).map { case rr => EC2Platform(pl) -> rr } match {
                  case JsSuccess(s, _) => Some(s)
                  case e: JsError => None // Silently ditch any records that do not have pricing
                }
            }.toMap
        }.toMap
        JsSuccess(p)
      }
    }
    (
      (JsPath \ "instance_type").read[String] and
      (JsPath \ "pretty_name").read[String] and
      (JsPath \ "memory").read[Double] and
      (JsPath \ "ECU").read[Double].orElse(Reads.pure(1.0)) and
      (JsPath \ "vCPU").read[Double] and
      (JsPath \ "storage").readNullable[EC2Storage] and
      (JsPath \ "arch").read[Seq[EC2Arch]] and
      (JsPath \ "network_performance").read[EC2NetworkPerformance] and
      (JsPath \ "enhanced_networking").read[Boolean] and
      (JsPath \ "vpc_only").read[Boolean] and
      (JsPath \ "linux_virtualization_types").read[Seq[EC2VirtualizationType]] and
      (JsPath \ "ebs_optimized").read[Boolean] and
      (JsPath \ "pricing").read[Map[EC2Region, Map[EC2Platform, EC2Pricing]]]
    )(EC2Instance.apply _)
  }
}

// Instance config with a set demand price (they are priced per region/platform). There are multiple zones inside each region.
// Spots are priced by zone. See InstanceKind.
trait EC2InstanceChoice {
  val instance: EC2Instance
  val region: EC2Region
  val platform: EC2Platform
  val price: EC2Pricing

  override def hashCode = instance.hashCode() + region.hashCode() + platform.hashCode() + price.hashCode()
  override def equals(that: Any) = that match {
    case t: EC2InstanceChoice => t.instance == instance && t.region == region && t.platform == platform && t.price == price
    case _ => false
  }

  override def toString = {
    s"EC2InstanceChoice(${instance.name}, ${region.name}, ${platform.name}, <price>)"
  }
}
object EC2InstanceChoice {
  def get(instance: EC2Instance)(region: EC2Region, platform: EC2Platform): Option[EC2InstanceChoice] = {
    instance.prices.get(region).flatMap(_.get(platform)).map { pricing =>
      val _instance = instance
      val _region = region
      val _platform = platform
      new EC2InstanceChoice {
        val instance = _instance
        val region = _region
        val platform = _platform
        val price = pricing
      }
    }
  }
}
