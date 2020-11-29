/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
