package scodec.protocols.mpeg

import scodec.Codec
import scodec.codecs._

case class PesPacketHeaderPrefix(streamId: Int, length: Int)

object PesPacketHeaderPrefix {

  implicit val codec: Codec[PesPacketHeaderPrefix] = {
    fixedSizeBytes(3,
      ("stream_id"                | uint8                ) ::
      ("pes_packet_length"        | uint16)
    ).as[PesPacketHeaderPrefix]
  }
}
