package scodec.protocols.mpeg
package transport
package psi

import scala.collection.immutable.IndexedSeq
import scalaz.\/
import scalaz.\/.{ left, right }
import scalaz.std.AllInstances._
import scodec.Codec
import scodec.bits._
import scodec.codecs._
import shapeless.Iso

case class ProgramAssociationTable(
  tsid: TransportStreamId,
  version: Int,
  current: Boolean,
  programByPid: Map[ProgramNumber, Pid]
)

object ProgramAssociationTable {

  private val MaxProgramsPerSection = 253

  def toSections(pat: ProgramAssociationTable): IndexedSeq[ProgramAssociationSection] = {
    val entries = pat.programByPid.toIndexedSeq.sortBy { case (ProgramNumber(n), _) => n }
    val groupedEntries = entries.grouped(MaxProgramsPerSection).toIndexedSeq
    groupedEntries.zipWithIndex.map { case (es, idx) =>
      ProgramAssociationSection(SectionExtension(pat.tsid.value, pat.version, pat.current, idx, groupedEntries.size), es)
    }
  }

  // TODO validate section data
  def fromSections(sections: IndexedSeq[ProgramAssociationSection]): String \/ ProgramAssociationTable = {
    if (sections.isEmpty) left("no sections")
    else right(ProgramAssociationTable(
      sections.head.tsid,
      sections.head.extension.version,
      sections.head.extension.current,
      (for {
        section <- sections
        pidMapping <- section.pidMappings
      } yield pidMapping).toMap
    ))
  }
}

case class ProgramAssociationSection(
  extension: SectionExtension,
  pidMappings: IndexedSeq[(ProgramNumber, Pid)]
) extends ExtendedSection {
  def tableId = ProgramAssociationSection.TableId
  def tsid: TransportStreamId = TransportStreamId(extension.tableIdExtension)
}

object ProgramAssociationSection {
  val TableId = 0

  implicit val iso = Iso.hlist(ProgramAssociationSection.apply _, ProgramAssociationSection.unapply _)

  private val dataCodec: Codec[IndexedSeq[(ProgramNumber, Pid)]] = {
    repeated {
      ("program_number" | Codec[ProgramNumber]) ~
      (reserved(3) ~>
      ("pid" | Codec[Pid]))
    }
  }

  implicit val sectionSubCodec: SectionSubCodec[ProgramAssociationSection] =
    SectionSubCodec.psi[ProgramAssociationSection, IndexedSeq[(ProgramNumber, Pid)]](
      TableId,
      (ext, mappings) => ProgramAssociationSection(ext, mappings),
      pat => (pat.extension, pat.pidMappings)
    )(dataCodec)
}
