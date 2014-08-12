package scodec.protocols.mpeg
package transport
package psi

import scala.collection.immutable.IndexedSeq
import scalaz.{ \/, NonEmptyList }
import scalaz.\/.{ left, right }
import scalaz.std.AllInstances._
import scodec.Codec
import scodec.bits._
import scodec.codecs._
import shapeless._

case class ConditionalAccessTable(
  version: Int,
  current: Boolean,
  descriptors: IndexedSeq[ConditionalAccessDescriptor]
)

object ConditionalAccessTable {

  def toSections(pat: ConditionalAccessTable): IndexedSeq[ConditionalAccessSection] = {
    ???
    /*
    val entries = pat.programByPid.toIndexedSeq.sortBy { case (ProgramNumber(n), _) => n }
    val groupedEntries = entries.grouped(MaxProgramsPerSection).toIndexedSeq
    groupedEntries.zipWithIndex.map { case (es, idx) =>
      ConditionalAccessSection(SectionExtension(pat.tsid.value, pat.version, pat.current, idx, groupedEntries.size), es)
    }
    */
  }

  def fromSections(sections: NonEmptyList[ConditionalAccessSection]): String \/ ConditionalAccessTable = {
    ???
  }
}

case class ConditionalAccessSection(
  extension: SectionExtension,
  descriptors: IndexedSeq[ConditionalAccessDescriptor]
) extends ExtendedSection {
  def tableId = ConditionalAccessSection.TableId
}

object ConditionalAccessSection {
  val TableId = 1

  type Fragment = IndexedSeq[ConditionalAccessDescriptor]

  private val fragmentCodec: Codec[Fragment] =
    repeated(Codec[ConditionalAccessDescriptor])

  implicit val sectionFragmentCodec: SectionFragmentCodec[ConditionalAccessSection] =
    SectionFragmentCodec.psi[ConditionalAccessSection, Fragment](
      TableId,
      (ext, descriptors) => ConditionalAccessSection(ext, descriptors),
      cat => (cat.extension, cat.descriptors)
    )(fragmentCodec)
}

case class ConditionalAccessDescriptor(systemId: Int, pid: Pid, privateData: BitVector)

object ConditionalAccessDescriptor {
  val Tag = 9

  implicit val codec: Codec[ConditionalAccessDescriptor] = {
    constant(Tag) ~>
    variableSizeBytes(uint8,
      ("ca_system_id" | uint16) ::
      (reserved(3) ~>
      ("ca_pid"       | Codec[Pid])) ::
      bits
    )
  }.as[ConditionalAccessDescriptor]

}
