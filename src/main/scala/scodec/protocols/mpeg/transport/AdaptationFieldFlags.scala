package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs._

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
