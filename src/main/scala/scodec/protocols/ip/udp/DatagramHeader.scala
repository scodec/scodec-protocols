package scodec.protocols
package ip
package udp

import scodec.bits.BitVector
import scodec.Codec
import scodec.codecs._

case class DatagramHeader(sourcePort: Port, destinationPort: Port, length: Int, checksum: Int)

object DatagramHeader {
  implicit val codec: Codec[DatagramHeader] = {
    val port = Codec[Port]
    ("source port"      | port) ::
    ("destination port" | port) ::
    ("length"           | uint16) ::
    ("checksum"         | uint16)
  }.as[DatagramHeader]
}
