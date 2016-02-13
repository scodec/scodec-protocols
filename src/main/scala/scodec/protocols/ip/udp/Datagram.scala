package scodec.protocols
package ip
package udp

import scodec.bits.BitVector
import scodec.{ Codec, DecodingContext }
import scodec.codecs._

case class Datagram(sourcePort: Port, destinationPort: Port, data: BitVector)

object Datagram {
  implicit val codec: Codec[Datagram] = new Codec[Datagram] {
    def sizeBound = Codec[DatagramHeader].sizeBound.atLeast

    def encode(dg: Datagram) = for {
      encHeader <- Codec.encode(DatagramHeader(dg.sourcePort, dg.destinationPort, 8 + dg.data.bytes.size.toInt, 0))
      chksum = checksum(encHeader ++ dg.data)
    } yield encHeader.dropRight(16) ++ chksum ++ dg.data

    def decode(b: BitVector) = (for {
      header <- DecodingContext(Codec[DatagramHeader])
      data <- DecodingContext(bits(8L * (header.length - 8)))
    } yield Datagram(header.sourcePort, header.destinationPort, data)).decode(b)
  }
}
