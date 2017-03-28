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
        ("adaptation_flags"       | Codec[AdaptationFieldFlags]                ) >>:~ { flags =>
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
