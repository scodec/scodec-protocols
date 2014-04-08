package scodec.protocols

import scodec.bits.ByteVector
import scodec.Codec
import scodec.codecs.bytes

case class MacAddress(value: ByteVector) {
  require(value.length == 6)
}

object MacAddress {
  implicit val codec: Codec[MacAddress] = bytes(6).xmap[MacAddress](v => MacAddress(v), _.value)
}
