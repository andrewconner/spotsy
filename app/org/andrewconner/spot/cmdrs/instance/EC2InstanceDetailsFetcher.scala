package org.andrewconner.spot.cmdrs.instance

import org.andrewconner.spot.core._
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext

class EC2InstanceDetailsFetcher(wsClient: WSClient, implicit val ec: ExecutionContext) {
  private lazy val fetch = {
    wsClient.url("https://raw.githubusercontent.com/powdahound/ec2instances.info/master/www/instances.json").get().map { resp =>
      implicit val eii = EC2Instance.eiiReads
      Json.parse(resp.body).as[Seq[EC2Instance]].map { instance =>
        instance.name -> instance
      }.toMap
    }
  }

  val value = fetch.asTask
}
