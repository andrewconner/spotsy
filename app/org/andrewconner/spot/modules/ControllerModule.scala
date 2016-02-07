package org.andrewconner.spot.modules

import com.softwaremill.macwire._
import org.andrewconner.spot.AppComponents
import org.andrewconner.spot.controllers.SpotController

trait ControllerModule { self: AppComponents =>

  // Dependencies
  //  def ec: ExecutionContext
  //  def wsClient: WSClient
  //  def awsClient: AWSClient
  //  def historicalSpotPrice: HistoricalSpotPrice

  // Controllers

  lazy val spotController: SpotController = wire[SpotController]
}
