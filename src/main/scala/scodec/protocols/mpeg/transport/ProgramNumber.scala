package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs.uint16

case class ProgramNumber(value: Int) {
  require(value >= ProgramNumber.MinValue && value <= ProgramNumber.MaxValue)
}

object ProgramNumber {
  val MinValue = 0
  val MaxValue = 65535

  implicit val codec: Codec[ProgramNumber] = uint16.xmap(ProgramNumber.apply, _.value)
}
