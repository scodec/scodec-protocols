package scodec
package protocols
package mpeg
package transport

import scodec.Codec
import scodec.codecs.uint

case class Pid(value: Int) {
  require(value >= Pid.MinValue && value <= Pid.MaxValue)
}

object Pid {
  val MinValue = 0
  val MaxValue = 8191

  implicit val codec: Codec[Pid] = uint(13).as[Pid]
}

case class PidStamped[+A](pid: Pid, value: A) {
  def map[B](f: A => B): PidStamped[B] = copy(value = f(value))
}

object PidStamped {

  /**
   * Combinator that converts a `Transform[S, I, O]` in to a `Transform[S, PidStamped[I], PidStamped[O]]` such that
   * pidstamps are preserved on elements that flow through the stream.
   */
  def preserve[S, I, O](t: Transform[S, I, O]): Transform[S, PidStamped[I], PidStamped[O]] =
    t.lens(_.value, (psi, o) => psi.copy(value = o))
}
