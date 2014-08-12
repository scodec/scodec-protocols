package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

/**
 * Partial modelling of the adaptation field.
 * The field extension, if present, is ignored upon decoding.
 */
case class AdaptationField(
  flags: AdaptationFieldFlags,
  pcr: Option[BitVector],
  opcr: Option[BitVector],
  spliceCountdown: Option[Int],
  transportPrivateData: Option[BitVector]
)

object AdaptationField {
  private val transportPrivateData: Codec[BitVector] =
    variableSizeBits(uint8, bits)

  implicit val codec: Codec[AdaptationField] = "adaptation_field" | {
    variableSizeBytes(uint8,
      ("adaptation_flags"       | Codec[AdaptationFieldFlags]                ) >>:~ { flags =>
      ("pcr"                    | conditional(flags.pcrFlag, bits(48))       ) ::
      ("opcr"                   | conditional(flags.opcrFlag, bits(48))      ) ::
      ("splice_countdown"       | conditional(flags.splicingPointFlag, int8) ) ::
      ("transport_private_data" | conditional(flags.transportPrivateDataFlag, transportPrivateData))
    })}.as[AdaptationField]
}
