package scodec.protocols
package ip
package udp

import scodec.bits.BitVector
import scodec.{ Codec, DecodingContext }
import scodec.codecs._
import shapeless._

case class Datagram(sourcePort: Port, destinationPort: Port, data: BitVector)

object Datagram {
  implicit val iso = Iso.hlist(Datagram.apply _, Datagram.unapply _)

  implicit val codec: Codec[Datagram] = new Codec[Datagram] {
    def encode(dg: Datagram) = for {
      encHeader <- Codec.encode(DatagramHeader(dg.sourcePort, dg.destinationPort, 8 + dg.data.bytes.size, 0))
      chksum = checksum(encHeader ++ dg.data)
    } yield encHeader.dropRight(16) ++ chksum ++ dg.data

    def decode(b: BitVector) = (for {
      header <- DecodingContext(Codec[DatagramHeader].decode)
      data <- DecodingContext(bits(8 * (header.length - 8)).decode)
    } yield Datagram(header.sourcePort, header.destinationPort, data)).run(b)
  }
}
