package org.andrewconner.spot

import _root_.controllers.Assets
import com.softwaremill.macwire._
import org.andrewconner.spot.core.time.Clock
import org.andrewconner.spot.modules._
import play.api.ApplicationLoader.Context
import play.api._
import play.api.libs.ws.WSClient
import play.api.libs.ws.ahc.AhcWSClient
import play.api.routing.Router

import scala.concurrent.ExecutionContext

class AppLoader extends ApplicationLoader {
  def load(context: Context) = {
    (new BuiltInComponentsFromContext(context) with AppComponents).application
  }
}

trait AppComponents
    extends BuiltInComponents
    with PlayAppModule
    with DatabaseModule // Database injection
    with DaoModule
    with ControllerModule // Application controllers
    with CmdrModule
    with CredentialsModule {

  implicit val ec: ExecutionContext = play.api.libs.concurrent.Execution.defaultContext // scala.concurrent.ExecutionContext.Implicits.global
  implicit val clock: Clock = new Clock()

  lazy val assets: Assets = wire[Assets]
  val prefix: String = "/"
  lazy val router: Router = wire[_root_.router.Routes].withPrefix(prefix)

  implicit val wsClient: WSClient = AhcWSClient()
  appShutdown.onStopAsync(wsClient.close())

}

