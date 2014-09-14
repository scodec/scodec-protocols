package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, NonEmptyList }
import scalaz.\/.{ left, right }
import scalaz.std.AllInstances._
import scodec.Codec
import scodec.bits._
import scodec.codecs._

case class ProgramAssociationTable(
  tsid: TransportStreamId,
  version: Int,
  current: Boolean,
  programByPid: Map[ProgramNumber, Pid]
)

object ProgramAssociationTable {

  val MaxProgramsPerSection = 253

  def toSections(pat: ProgramAssociationTable): Vector[ProgramAssociationSection] = {
    val entries = pat.programByPid.toVector.sortBy { case (ProgramNumber(n), _) => n }
    val groupedEntries = entries.grouped(MaxProgramsPerSection).toVector
    groupedEntries.zipWithIndex.map { case (es, idx) =>
      ProgramAssociationSection(SectionExtension(pat.tsid.value, pat.version, pat.current, idx, groupedEntries.size), es)
    }
  }

  // TODO validate section data
  def fromSections(sections: NonEmptyList[ProgramAssociationSection]): String \/ ProgramAssociationTable = {
    right(ProgramAssociationTable(
      sections.head.tsid,
      sections.head.extension.version,
      sections.head.extension.current,
      (for {
        section <- sections.list
        pidMapping <- section.pidMappings
      } yield pidMapping).toMap
    ))
  }
}

case class ProgramAssociationSection(
  extension: SectionExtension,
  pidMappings: Vector[(ProgramNumber, Pid)]
) extends ExtendedSection {
  def tableId = ProgramAssociationSection.TableId
  def tsid: TransportStreamId = TransportStreamId(extension.tableIdExtension)
}

object ProgramAssociationSection {
  val TableId = 0

  private type Fragment = Vector[(ProgramNumber, Pid)]

  private val fragmentCodec: Codec[Fragment] = {
    vector {
      ("program_number" | Codec[ProgramNumber]) ~
      (reserved(3) ~>
      ("pid" | Codec[Pid]))
    }
  }

  implicit val sectionFragmentCodec: SectionFragmentCodec[ProgramAssociationSection] =
    SectionFragmentCodec.psi[ProgramAssociationSection, Vector[(ProgramNumber, Pid)]](
      TableId,
      (ext, mappings) => ProgramAssociationSection(ext, mappings),
      pat => (pat.extension, pat.pidMappings)
    )(fragmentCodec)
}
