package org.andrewconner.spot.modules

import com.sun.media.sound.ModelSource
import org.andrewconner.spot.AppComponents
import org.andrewconner.spot.playapp.TarpitFilter
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.libs.ws.ahc.AhcWSClient
import play.api.mvc.EssentialFilter
import com.softwaremill.macwire._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

trait PlayAppModule { self: AppComponents =>

  lazy val appShutdown: AppShutdown = new AppShutdown(applicationLifecycle)
  override lazy val httpFilters: Seq[EssentialFilter] = if (environment.mode == play.api.Mode.Test) {
    Seq()
  } else {
    Seq(wire[TarpitFilter])
  }

}

class AppShutdown(applicationLifecycle: ApplicationLifecycle)(implicit val ec: ExecutionContext) {
  // On app shutdown, calls `f` on a separate thread, not blocking the rest of application shutdown.
  // Useful when we really don't care about the result.

  def onStopAsync(f: => Unit): Unit = {
    applicationLifecycle.addStopHook { () =>
      Future { f }(ec).onComplete {
        case Success(s) =>
          Logger.info("Success stopping")
        case Failure(ex) =>
          Logger.error("Error executing stop hook", ex)
      }
      Future.successful(())
    }
  }

  def onStopBlock(f: => Unit) = {
    applicationLifecycle.addStopHook { () =>
      Future.successful(f)
    }
  }

  def onStartAsync(f: => Unit) = {
    println("")
  }
}
