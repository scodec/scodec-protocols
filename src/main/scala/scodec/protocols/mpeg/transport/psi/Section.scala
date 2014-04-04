package scodec.protocols.mpeg
package transport
package psi

import scalaz.std.AllInstances._
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.Iso

trait Section {
  def tableId: Int
}

trait ExtendedSection extends Section {
  def extension: SectionExtension
}

case class SectionExtension(
  tableIdExtension: Int,
  version: Int,
  current: Boolean,
  sectionNumber: Int,
  lastSectionNumber: Int
)

object SectionExtension {
  implicit val iso = Iso.hlist(SectionExtension.apply _, SectionExtension.unapply _)

  implicit val codec: Codec[SectionExtension] = {
    ("table_id_extension"     | uint16) ::
    reserved(2) :~>:
    ("version_number"         | uint(5)) ::
    ("current_next_indicator" | bool) ::
    ("section_number"         | uint8) ::
    ("last_section_number"    | uint8)
  }.as[SectionExtension]
}
