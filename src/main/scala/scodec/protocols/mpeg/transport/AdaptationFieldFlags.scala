package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.Iso

/** Flags in the adaptation field. */
case class AdaptationFieldFlags(
  discontinuity: Boolean,
  randomAccess: Boolean,
  priority: Boolean,
  pcrFlag: Boolean,
  opcrFlag: Boolean,
  splicingPointFlag: Boolean,
  transportPrivateDataFlag: Boolean,
  adaptationFieldExtension: Boolean)

object AdaptationFieldFlags {
  implicit def iso = Iso.hlist(AdaptationFieldFlags.apply _, AdaptationFieldFlags.unapply _)

  implicit val codec: Codec[AdaptationFieldFlags] = "adaptation_field_flags" | fixedSizeBytes(1,
    ("discontinuity"             | bool                    ) ::
    ("randomAccess"              | bool                    ) ::
    ("priority"                  | bool                    ) ::
    ("pcrFlag"                   | bool                    ) ::
    ("opcrFlag"                  | bool                    ) ::
    ("splicingPointFlag"         | bool                    ) ::
    ("transportPrivateDataFlag"  | bool                    ) ::
    ("adaptationFieldExtension"  | bool                    )
  ).as[AdaptationFieldFlags]
}
