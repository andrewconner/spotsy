package org.andrewconner.spot.core

import java.util.concurrent.TimeoutException

import akka.util.ByteString
import play.api.Logger
import play.api.libs.iteratee.Execution.Implicits._
import play.api.libs.streams.Accumulator
import play.api.mvc._

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scalaz.concurrent.Task

trait TaskAction[A] extends Action[A] {

  def applyT(request: Request[A]): Task[Result]

  // Implementation:

  def apply(request: Request[A]): Future[Result] = {
    applyT(request).runFuture()
  }

  override def apply(rh: RequestHeader): Accumulator[ByteString, Result] = {
    parser(rh).mapFuture { ei =>
      val result = ei match {
        case Left(r) => Task.now(r)
        case Right(a) => applyT(Request(rh, a))
      }
      val itFut = result.runFuture()
      itFut
    }(defaultExecutionContext)
  }
}

trait TaskActionFunction[-R[_], +P[_]] extends ActionFunction[R, P] { self =>
  def invokeBlockT[A](request: R[A], block: P[A] => Task[Result]): Task[Result]

  def invokeBlock[A](request: R[A], block: P[A] => Future[Result]): Future[Result] = {
    invokeBlockT(request, block.andThen(_.asTask)).runFuture()
  }

  override def andThen[Q[_]](other: ActionFunction[P, Q]): ActionFunction[R, Q] = new TaskActionFunction[R, Q] {
    def invokeBlockT[A](request: R[A], block: Q[A] => Task[Result]) = {
      other match {
        case t: TaskActionFunction[P, Q] =>
          self.invokeBlockT[A](request, t.invokeBlockT[A](_, block))
        case o: ActionFunction[P, Q] =>
          self.invokeBlock[A](request, o.invokeBlock[A](_, block.andThen(_.runFuture()))).asTask
      }
    }
  }
}
trait TaskActionBuilder[+R[_]] extends TaskActionFunction[Request, R] { self =>

  // Task APIs
  def applyT[A](bodyParser: BodyParser[A])(block: R[A] => Result): Action[A] = task(bodyParser) { req: R[A] =>
    Task.fork(Task.now(block(req)))
  }
  def applyT(block: R[AnyContent] => Result): Action[AnyContent] = applyT(BodyParsers.parse.default)(block)
  def applyT(block: => Result): Action[AnyContent] = applyT(_ => block)
  def task(block: => Task[Result]): Action[AnyContent] = task(_ => block)
  def task(block: R[AnyContent] => Task[Result]): Action[AnyContent] = task(BodyParsers.parse.default)(block)
  def task[A](bodyParser: BodyParser[A])(block: R[A] => Task[Result]): Action[A] = composeAction(new TaskAction[A] {
    def parser = composeParser(bodyParser)
    override def apply(request: Request[A]) = try {
      // This is opinionated in that the blocks should be run in another thread. This may not be what you want, though.
      Task.fork(invokeBlockT(request, block)).runFuture()
    } catch {
      case e: NotImplementedError => throw new RuntimeException(e)
      case e: LinkageError => throw new RuntimeException(e)
    }

    def applyT(request: Request[A]) = try {
      invokeBlockT(request, block)
    } catch {
      case e: NotImplementedError => throw new RuntimeException(e)
      case e: LinkageError => throw new RuntimeException(e)
    }
  })

  protected def composeParser[A](bodyParser: BodyParser[A]): BodyParser[A] = bodyParser
  protected def composeAction[A](action: Action[A]): Action[A] = action
  override def andThen[Q[_]](other: ActionFunction[R, Q]): TaskActionBuilder[Q] = new TaskActionBuilder[Q] {
    def invokeBlockT[A](request: Request[A], block: Q[A] => Task[Result]) = {
      other match {
        case t: TaskActionFunction[R, Q] =>
          self.invokeBlockT[A](request, t.invokeBlockT[A](_, block))
        case o: ActionFunction[R, Q] =>
          self.invokeBlock[A](request, o.invokeBlock[A](_, block.andThen(_.runFuture()))).asTask
      }
    }
    override protected def composeParser[A](bodyParser: BodyParser[A]): BodyParser[A] = self.composeParser(bodyParser)
    override protected def composeAction[A](action: Action[A]): Action[A] = self.composeAction(action)
  }
}

object TaskAction extends TaskActionBuilder[Request] {
  private val logger = Logger(Action.getClass)

  def invokeBlockT[A](request: Request[A], block: (Request[A]) => Task[Result]) = block(request)
}

trait TaskActionRefiner[-R[_], +P[_]] extends TaskActionFunction[R, P] {
  protected def refine[A](request: R[A]): Task[scalaz.\/[Result, P[A]]]

  final def invokeBlockT[A](request: R[A], block: P[A] => Task[Result]) = {
    refine(request).flatMap(_.fold(Task.now, block))
  }
}

trait TaskActionTransformer[-R[_], +P[_]] extends TaskActionRefiner[R, P] {
  protected def transform[A](request: R[A]): Task[P[A]]

  final def refine[A](request: R[A]) =
    transform(request).map(scalaz.\/-(_))
}

trait TaskActionFilter[R[_]] extends TaskActionRefiner[R, R] {
  protected def filter[A](request: R[A]): Task[Option[Result]]

  final protected def refine[A](request: R[A]) =
    filter(request).map {
      case Some(rejection) => scalaz.-\/(rejection)
      case None => scalaz.\/-(request)
    }
}

case class TimedTaskAction(timeout: Duration) extends TaskActionBuilder[Request] {
  def invokeBlockT[A](request: Request[A], block: (Request[A] => Task[Result])) = {
    block(request).unsafePerformTimed(timeout).handle {
      case t: TimeoutException =>
        t.printStackTrace()
        Results.Ok("timedout")
    }
  }
}