package scodec
package protocols
package mpeg
package transport

import scodec.Codec
import scodec.codecs.uint

import scalaz.{ Lens, LensFamily }
import scalaz.stream.Process1

case class Pid(value: Int) {
  require(value >= Pid.MinValue && value <= Pid.MaxValue)
}

object Pid {
  val MinValue = 0
  val MaxValue = 8191

  implicit val codec: Codec[Pid] = uint(13).xmap(Pid.apply, _.value)
}

case class PidStamped[+A](pid: Pid, value: A)

object PidStamped {

  object Lenses {
    def Pid[A]: Lens[PidStamped[A], Pid] = Lens.lensu((ps, p) => ps.copy(pid = p), _.pid)
    def Value[A]: Lens[PidStamped[A], A] = Lens.lensu((ps, a) => ps.copy(value = a), _.value)

    def ValueMap[A, B]: LensFamily[PidStamped[A], PidStamped[B], A, B] =
      Lens.lensFamilyu((psa, b) => PidStamped(psa.pid, b), _.value)
  }

  /**
   * Combinator that converts a `Process1[A, B]` in to a `Process1[PidStamped[A], PidStamped[B]]` such that
   * pidstamps are preserved on elements that flow through the process.
   */
  def preservePidStamps[A, B](p: Process1[A, B]): Process1[PidStamped[A], PidStamped[B]] =
    process1ext.lensf(Lenses.ValueMap[A, B])(p)
}
