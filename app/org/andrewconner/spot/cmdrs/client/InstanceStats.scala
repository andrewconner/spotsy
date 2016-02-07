package org.andrewconner.spot.cmdrs.client

import org.andrewconner.spot.cmdrs.instance._

// Client-facing instance information
case class InstanceStats(
  // Instance information, from EC2Instance
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

  // Choice information
  zone: String,
  region: EC2Region,
  platform: EC2Platform,
  prices: EC2Pricing,

  // Spot info
  spotStatsThreshhold: Double,
  weeklySpotStats: SpotPriceStats,
  allTimeSpotStats: SpotPriceStats,
  pricePastDay: Option[Double],
  pricePastWeek: Option[Double],
  priceSamples: Int,
  periods: Int
)

case class SpotPriceStats(
  avg: Double, // average price over all history window
  max: Double, // max price over all history window
  perECU: Double, // based on threshold cost
  perGbRam: Double, // based on threshold cost
  discount: Double, // discount of threshold cost
  price: Double // price such that instance stays up at least threshold of the time
)
