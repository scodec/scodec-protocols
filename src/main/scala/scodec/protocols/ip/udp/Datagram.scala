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

  implicit val codec: Codec[Datagram] = {
    val port = Codec[Port]

    val headerCodec =
      ("source port"      | port) ::
      ("destination port" | port) ::
      ("length"           | uint16) ::
      ("checksum"         | uint16)

    new Codec[Datagram] {
      def encode(dg: Datagram) = for {
        encHeader <- headerCodec.encode(dg.sourcePort :: dg.destinationPort :: (8 + (dg.data.size / 8).toInt) :: 0 :: HNil)
        chksum = checksum(encHeader ++ dg.data)
      } yield encHeader.dropRight(16) ++ chksum ++ dg.data

      def decode(b: BitVector) = (for {
        header <- DecodingContext(headerCodec.decode)
        src :: dst :: length :: chksum :: HNil = header
        data <- DecodingContext(bits(8 * (length - 8)).decode)
      } yield Datagram(src, dst, data)).run(b)
    }

  }
}
