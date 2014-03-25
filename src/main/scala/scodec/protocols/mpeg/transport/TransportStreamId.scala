package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs.uint16

case class TransportStreamId(value: Int) {
  require(value >= TransportStreamId.MinValue && value <= TransportStreamId.MaxValue)
}

object TransportStreamId {
  val MinValue = 0
  val MaxValue = 65535

  implicit val codec: Codec[TransportStreamId] = uint16.xmap(TransportStreamId.apply, _.value)
}
