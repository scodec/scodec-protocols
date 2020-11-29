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
package transport

import scodec.{ Attempt, Codec, DecodeResult, SizeBound }
import scodec.bits.BitVector
import scodec.codecs._

/**
 * Partial modelling of the adaptation field.
 * The field extension, if present, is ignored upon decoding.
 */
case class AdaptationField(
  flags: Option[AdaptationFieldFlags],
  pcr: Option[BitVector],
  opcr: Option[BitVector],
  spliceCountdown: Option[Int],
  transportPrivateData: Option[BitVector]
)

object AdaptationField {

  final val Empty: AdaptationField = AdaptationField(None, None, None, None, None)

  implicit val codec: Codec[AdaptationField] = new Codec[AdaptationField] {
    private case class NonEmptyAF(
      flags: AdaptationFieldFlags,
      pcr: Option[BitVector],
      opcr: Option[BitVector],
      spliceCountdown: Option[Int],
      transportPrivateData: Option[BitVector]
    ) { def asAF: AdaptationField = AdaptationField(Some(flags), pcr, opcr, spliceCountdown, transportPrivateData) }

    private val transportPrivateData: Codec[BitVector] = variableSizeBits(uint8, bits)
    private val nonEmptyAFCodec: Codec[NonEmptyAF] = "adaptation_field" | {
      variableSizeBytes(uint8,
        ("adaptation_flags"       | Codec[AdaptationFieldFlags]                ).flatPrepend { flags =>
        ("pcr"                    | conditional(flags.pcrFlag, bits(48))       ) ::
        ("opcr"                   | conditional(flags.opcrFlag, bits(48))      ) ::
        ("splice_countdown"       | conditional(flags.splicingPointFlag, int8) ) ::
        ("transport_private_data" | conditional(flags.transportPrivateDataFlag, transportPrivateData))
      })
    }.as[NonEmptyAF]

    def sizeBound: SizeBound = SizeBound.unknown

    def encode(af: AdaptationField): Attempt[BitVector] = af.flags.fold(uint8.encode(0)) { flags =>
      nonEmptyAFCodec.encode(NonEmptyAF(flags, af.pcr, af.opcr, af.spliceCountdown, af.transportPrivateData))
    }

    def decode(bv: BitVector): Attempt[DecodeResult[AdaptationField]] = uint8.decode(bv) flatMap { size =>
      if (size.value > 0) nonEmptyAFCodec.decode(bv).map(_.map(_.asAF)) else Attempt.successful(DecodeResult(Empty, size.remainder))
    }
  }
}
