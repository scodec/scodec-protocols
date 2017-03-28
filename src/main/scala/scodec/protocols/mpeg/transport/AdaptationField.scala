package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

import shapeless.{ ::, HNil }

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
  private val transportPrivateData: Codec[BitVector] =
    variableSizeBits(uint8, bits)

  implicit val codec: Codec[AdaptationField] = {
    val fields = {
      "adaptation_field" | {
        ("field_size"               | uint8                                                  ) >>:~ { fieldSize =>
        variableSizeBytes(provide(fieldSize),
          ("adaptation_flags"       | conditional(fieldSize > 0, Codec[AdaptationFieldFlags])) >>:~ { flags =>
          ("pcr"                    | conditional(flags.exists(_.pcrFlag), bits(48))         ) ::
          ("opcr"                   | conditional(flags.exists(_.opcrFlag), bits(48))        ) ::
          ("splice_countdown"       | conditional(flags.exists(_.splicingPointFlag), int8)   ) ::
          ("transport_private_data" | conditional(flags.exists(_.transportPrivateDataFlag), transportPrivateData))
        })}
      }
    }
    def calcFieldSize(
      flags: Option[AdaptationFieldFlags],
      pcr: Option[BitVector],
      opcr: Option[BitVector],
      scd: Option[Int],
      tpd: Option[BitVector]
    ): Int = flags.fold(0) { _ =>
      pcr.fold(0) { _.bytes.size.toInt } + opcr.fold(0) { _.bytes.size.toInt } + scd.fold(0) { _ => 1 } + tpd.fold(0) { _.bytes.size.toInt }
    }
    fields.xmapc {
      case fieldSize :: flags :: pcr :: opcr :: scd :: tpd :: HNil => flags :: pcr :: opcr :: scd :: tpd :: HNil
    } {
      case flags :: pcr :: opcr :: scd :: tpd :: HNil => calcFieldSize(flags, pcr, opcr, scd, tpd) :: flags :: pcr :: opcr :: scd :: tpd :: HNil
    }.as[AdaptationField]
  }
}
