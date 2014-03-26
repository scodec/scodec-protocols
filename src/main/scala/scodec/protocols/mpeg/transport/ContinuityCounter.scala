package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs.uint

case class ContinuityCounter(value: Int) {
  require(value >= ContinuityCounter.MinValue && value <= ContinuityCounter.MaxValue)

  def next: ContinuityCounter = ContinuityCounter((value + 1) % 16)
}

object ContinuityCounter {
  val MinValue = 0
  val MaxValue = 15

  implicit val codec: Codec[ContinuityCounter] = uint(4).xmap(ContinuityCounter.apply, _.value)
}
