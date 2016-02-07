package org.andrewconner.spot.modules

import com.amazonaws.auth.AWSCredentials
import com.amazonaws.regions.Regions
import com.softwaremill.macwire.MacwireMacros._
import com.softwaremill.macwire._
import org.andrewconner.spot.AppComponents
import org.andrewconner.spot.cmdrs.instance.EC2InstanceDetailsFetcher
import org.andrewconner.spot.cmdrs.{ InstanceHistoryBuilder, HistoricalSpotPrice, AWSClient }

import scala.concurrent.ExecutionContext

trait CmdrModule { self: AppComponents =>

  lazy val awsClient: AWSClient = wire[AWSClient]

  lazy val historicalSpotPrice: HistoricalSpotPrice = wire[HistoricalSpotPrice]
  lazy val ec2InstanceDetailsFetcher: EC2InstanceDetailsFetcher = wire[EC2InstanceDetailsFetcher]
  lazy val instanceHistoryBuilder: InstanceHistoryBuilder = wire[InstanceHistoryBuilder]

}
