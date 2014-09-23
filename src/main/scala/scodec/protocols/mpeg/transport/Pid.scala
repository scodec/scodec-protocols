package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs.uint

case class Pid(value: Int) {
  require(value >= Pid.MinValue && value <= Pid.MaxValue)
}

object Pid {
  val MinValue = 0
  val MaxValue = 8191

  implicit val codec: Codec[Pid] = uint(13).xmap(Pid.apply, _.value)
}

case class PidStamped[A](pid: Pid, value: A)
