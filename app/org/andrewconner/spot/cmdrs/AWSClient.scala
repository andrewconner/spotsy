package org.andrewconner.spot.cmdrs

import cats.Later
import com.amazonaws.AmazonWebServiceRequest
import com.amazonaws.auth.AWSCredentials
import com.amazonaws.handlers.AsyncHandler
import com.amazonaws.regions.{ Regions, Region }
import com.amazonaws.services.ec2.AmazonEC2AsyncClient
import com.amazonaws.services.ec2.model.{ DescribeSpotPriceHistoryResult, DescribeSpotPriceHistoryRequest }
import org.andrewconner.spot.modules.AppShutdown
import org.joda.time.DateTime
import org.andrewconner.spot.core._
import play.api.inject.ApplicationLifecycle

import scala.concurrent.{ ExecutionContext, Future }
import scalaz.Memo
import scalaz.concurrent.Task
import scalaz.Scalaz._

class AWSClient(credentials: AWSCredentials, appShutdown: AppShutdown)(implicit val ec: ExecutionContext) {

  val clients = Regions.values().map { r =>
    r -> Lazily {
      val client = new AmazonEC2AsyncClient(credentials)
      client.setRegion(Region.getRegion(r))
      client
    }
  }.toMap

  appShutdown.onStopAsync {
    clients.foreach(_._2.foreach(_.shutdown()))
  }

  def fetchSpotPrices(region: Regions)(request: DescribeSpotPriceHistoryRequest, nextToken: String): Task[DescribeSpotPriceHistoryResult] = {
    Task.async[DescribeSpotPriceHistoryResult] { k =>
      clients(region).get.describeSpotPriceHistoryAsync(request.clone().withNextToken(Option(nextToken).getOrElse("")), asyncHandler[DescribeSpotPriceHistoryRequest, DescribeSpotPriceHistoryResult](k))
      ()
    }
  }

  private def asyncHandler[R <: AmazonWebServiceRequest, T](register: scalaz.\/[Throwable, T] => Unit): AsyncHandler[R, T] = {
    new AsyncHandler[R, T] {
      def onSuccess(req: R, res: T) = register(res.right)
      def onError(ex: Exception) = register(ex.left)
    }
  }

}
