package scodec.protocols.mpeg

import scodec.bits._
import scodec.{ Attempt, Decoder, DecodeResult, Err }

trait PesPacket

object PesPacket {

  case class WithHeader(streamId: Int, header: PesPacketHeader, data: BitVector) extends PesPacket
  case class WithoutHeader(streamId: Int, data: BitVector) extends PesPacket
  case object Padding extends PesPacket

  def decode(prefix: PesPacketHeaderPrefix, buffer: BitVector): Attempt[DecodeResult[PesPacket]] =
    decoder(prefix).decode(buffer)

  def decoder(prefix: PesPacketHeaderPrefix): Decoder[PesPacket] = Decoder { buffer =>
    val id = prefix.streamId
    import PesStreamId._
    if (id != ProgramStreamMap &&
        id != PaddingStream &&
        id != PrivateStream2 &&
        id != ECM &&
        id != EMM &&
        id != ProgramStreamDirectory &&
        id != DSMCC &&
        id != `ITU-T Rec. H.222.1 type E`) {
      PesPacketHeader.codec.decode(buffer) match {
        case Attempt.Successful(DecodeResult(header, rest)) =>
          decodeWithHeader(prefix, header, rest)
        case f @ Attempt.Failure(_) => f
      }
    } else if (
      id == ProgramStreamMap ||
      id == PrivateStream2 ||
      id == ECM ||
      id == EMM |
      id == ProgramStreamDirectory ||
      id == DSMCC ||
      id == `ITU-T Rec. H.222.1 type E`) {
      Attempt.successful(DecodeResult(WithoutHeader(id, buffer), BitVector.empty))
    } else if (id == PaddingStream) {
      Attempt.successful(DecodeResult(Padding, BitVector.empty))
    } else {
      Attempt.failure(Err(s"Unknown PES stream id: $id"))
    }
  }

  def decodeWithHeader(prefix: PesPacketHeaderPrefix, header: PesPacketHeader, data: BitVector): Attempt[DecodeResult[PesPacket]] = {
    Attempt.successful(DecodeResult(WithHeader(prefix.streamId, header, data), BitVector.empty))
  }
}
