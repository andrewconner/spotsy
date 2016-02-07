package org.andrewconner.spot.core

final class Lazily[A](f: () => A) {
  private[this] var thunk: () => A = f
  @volatile private var inst = false
  private lazy val value: A = {
    val r = thunk()
    thunk = null // scalastyle:off
    inst = true
    r
  }
  def get: A = value
  def isDefined = inst
  def isEmpty = !inst
  def nonEmpty = inst

  def map[B](g: => A => B): Lazily[B] = Lazily(g(get))

  def foreach(f: A => Unit): Unit = {
    if (isDefined) f(get)
    else ()
  }

  def flatMap[B](b: => A => Lazily[B]): Lazily[B] = b(get)
}

object Lazily {
  def apply[T](f: => T) = new Lazily(() => f)
}