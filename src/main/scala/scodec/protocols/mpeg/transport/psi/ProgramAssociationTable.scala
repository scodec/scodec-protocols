package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, NonEmptyList, Tag, Tags }
import scalaz.\/.{ left, right }
import scalaz.std.AllInstances._
import scalaz.syntax.all._
import scalaz.syntax.std.option._

import scalaz.stream.{ Process, Process1 }

import scodec.Codec
import scodec.bits._
import scodec.codecs._

case class ProgramAssociationTable(
  tsid: TransportStreamId,
  version: Int,
  current: Boolean,
  programByPid: Map[ProgramNumber, Pid]
) extends Table {
  def tableId = ProgramAssociationSection.TableId
  def toSections: NonEmptyList[ProgramAssociationSection] = ProgramAssociationTable.toSections(this)
}

object ProgramAssociationTable {

  val MaxProgramsPerSection = 253

  def toSections(pat: ProgramAssociationTable): NonEmptyList[ProgramAssociationSection] = {
    val entries = pat.programByPid.toVector.sortBy { case (ProgramNumber(n), _) => n }
    val groupedEntries = entries.grouped(MaxProgramsPerSection).toVector
    val lastSection = groupedEntries.size - 1
    val sections = groupedEntries.zipWithIndex.map { case (es, idx) =>
      ProgramAssociationSection(SectionExtension(pat.tsid.value, pat.version, pat.current, idx, lastSection), es)
    }
    if (sections.isEmpty)
      NonEmptyList(ProgramAssociationSection(SectionExtension(pat.tsid.value, pat.version, pat.current, 0, 0), Vector.empty))
    else
      NonEmptyList(sections.head, sections.tail: _*)
  }

  def fromSections(sections: NonEmptyList[ProgramAssociationSection]): String \/ ProgramAssociationTable = {
    def extract[A](name: String, f: ProgramAssociationSection => A): String \/ A = {
      val extracted = sections.map(f).list.distinct
      if (extracted.size == 1) right(extracted.head)
      else left(s"sections have diferring $name: " + extracted.mkString(", "))
    }
    for {
      tsid <- extract("TSIDs", _.tsid)
      version <- extract("versions", _.extension.version)
      current = Tag.unwrap(sections.foldMap { p => Tags.Disjunction(p.extension.current) })
    } yield ProgramAssociationTable(
      tsid,
      version,
      current,
      (for {
        section <- sections.list
        pidMapping <- section.pidMappings
      } yield pidMapping).toMap
    )
  }

  implicit val tableSupport: TableSupport[ProgramAssociationTable] = new TableSupport[ProgramAssociationTable] {
    def tableId = ProgramAssociationSection.TableId
    def toTable(gs: GroupedSections) =
      gs.as[ProgramAssociationSection].toRightDisjunction(s"Not PAT sections").flatMap { sections => fromSections(sections) }
    def toSections(pat: ProgramAssociationTable) = ProgramAssociationTable.toSections(pat)
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
