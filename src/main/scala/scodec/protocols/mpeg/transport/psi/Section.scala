package scodec.protocols.mpeg
package transport
package psi

import scodec.Codec
import scodec.codecs._

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
  implicit val codec: Codec[SectionExtension] = {
    ("table_id_extension"     | uint16) ::
    reserved(2) :~>:
    ("version_number"         | uint(5)) ::
    ("current_next_indicator" | bool) ::
    ("section_number"         | uint8) ::
    ("last_section_number"    | uint8)
  }.as[SectionExtension]
}
