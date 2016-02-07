package org.andrewconner.spot.cmdrs.instance

sealed trait InstanceFilter {
  type Elem
  val accessor: EC2Instance => Elem
  def filter(elem: Elem): Boolean

  def run(instance: EC2Instance) = filter(accessor(instance))
}

object InstanceFilter {
  def memory(min: Option[Double], max: Option[Double]): InstanceFilter = RangeFilter(min, max)(_.memory)
  def ecu(min: Option[Double], max: Option[Double]): InstanceFilter = RangeFilter(min, max)(_.ecu)
  def cpu(min: Option[Double], max: Option[Double]): InstanceFilter = RangeFilter(min, max)(_.cores)
  def storageSize(min: Option[Double], max: Option[Double]): InstanceFilter = RangeFilter(min, max)(_.storage.map(_.size).getOrElse(0.0))
  def storageSsd(choice: Option[Boolean]): InstanceFilter = BooleanFilter(choice)(_.storage.exists(_.ssd))
  def arch(choices: Option[Set[EC2Arch]]): InstanceFilter = SeqSetFilter(choices)(_.arch)
  def networkPerformance(choices: Option[Set[EC2NetworkPerformance]]): InstanceFilter = SetFilter(choices)(_.networkPerformance)
  def enhancedNetworking(choice: Option[Boolean]): InstanceFilter = BooleanFilter(choice)(_.enhancedNetworking)
  def vpcOnly(choice: Option[Boolean]): InstanceFilter = BooleanFilter(choice)(_.vpcOnly)
  def linuxVirtualizationTypes(choices: Option[Set[EC2VirtualizationType]]): InstanceFilter = SeqSetFilter(choices)(_.linuxVirtualizationType)
  def ebsOptimized(choice: Option[Boolean]): InstanceFilter = BooleanFilter(choice)(_.ebsOptimized)

  // min & max given, filter based on range
  case class RangeFilter(min: Option[Double], max: Option[Double])(val accessor: EC2Instance => Double) extends InstanceFilter {
    type Elem = Double
    def filter(elem: Double) = {
      (min, max) match {
        case (None, None) => true
        case (Some(m1), None) => elem >= m1
        case (None, Some(m2)) => elem <= m2
        case (Some(m1), Some(m2)) => elem >= m1 && elem <= m2
      }
    }
  }

  // Set of valid choices given, filter single element
  case class SetFilter[T](choices: Option[Set[T]])(val accessor: EC2Instance => T) extends InstanceFilter {
    type Elem = T
    def filter(elem: T) = {
      choices match {
        case None => true
        case Some(set) => set.contains(elem)
      }
    }
  }

  // Set of valid choices given, filter such that a choice exists in elems
  case class SeqSetFilter[T](choices: Option[Set[T]])(val accessor: EC2Instance => Seq[T]) extends InstanceFilter {
    type Elem = Seq[T]
    def filter(elems: Seq[T]) = {
      choices match {
        case None => true
        case Some(set) => set.exists(elems.contains)
      }
    }
  }

  case class BooleanFilter[T](choice: Option[Boolean])(val accessor: EC2Instance => Boolean) extends InstanceFilter {
    type Elem = Boolean
    def filter(elem: Boolean) = {
      choice match {
        case None => true
        case Some(c) => elem == c
      }
    }
  }
}

