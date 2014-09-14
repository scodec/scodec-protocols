package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, NonEmptyList }
import scalaz.\/.{ left, right }
import scalaz.std.AllInstances._
import scodec.Codec
import scodec.bits._
import scodec.codecs._
import shapeless._

case class ProgramMapTable(
  programNumber: ProgramNumber,
  version: Int,
  current: Boolean,
  pcrPid: Pid,
  programInfoDescriptors: BitVector,
  componentStreamMapping: Map[StreamType, ProgramMapRecord]
)

object ProgramMapTable {

  def toSection(pmt: ProgramMapTable): ProgramMapSection = {
    ProgramMapSection(
      SectionExtension(pmt.programNumber.value, pmt.version, pmt.current, 0, 0),
      pmt.pcrPid,
      pmt.programInfoDescriptors,
      pmt.componentStreamMapping.toVector.sortBy { case (k, v) => k.value }
    )
  }

  def fromSection(section: ProgramMapSection): ProgramMapTable = {
    ProgramMapTable(
      section.programNumber,
      section.extension.version,
      section.extension.current,
      section.pcrPid,
      section.programInfoDescriptors,
      section.componentStreamMapping.toMap
    )
  }
}

case class StreamType(value: Int)
case class ProgramMapRecord(pid: Pid, descriptors: BitVector)
object ProgramMapRecord {
  //def apply(pid: Pid) = new ProgramMapRecord(pid, BitVector.empty)
}

case class ProgramMapSection(
  extension: SectionExtension,
  pcrPid: Pid,
  programInfoDescriptors: BitVector,
  componentStreamMapping: Vector[(StreamType, ProgramMapRecord)]
) extends ExtendedSection {
  def tableId = ProgramMapSection.TableId
  def programNumber: ProgramNumber = ProgramNumber(extension.tableIdExtension)
}

object ProgramMapSection {
  val TableId = 2

  private type Fragment = Pid :: BitVector :: Vector[(StreamType, ProgramMapRecord)] :: HNil
  private val fragmentCodec: Codec[Fragment] = {
    def pid: Codec[Pid] = reserved(3) ~> Codec[Pid]
    def descriptor: Codec[BitVector] =
      reserved(4) ~> variableSizeBytes(uint(12), bits)
    def programMapRecord: Codec[ProgramMapRecord] =
      (("pid" | pid) :: ("es_descriptors" | descriptor)).as[ProgramMapRecord]

    ("pcr_pid" | pid) ::
    ("program_info_descriptors" | descriptor) ::
    vector {
      ("stream_type" | uint8.as[StreamType]) ~ programMapRecord
    }
  }

  implicit val sectionSubCodec: SectionFragmentCodec[ProgramMapSection] =
    SectionFragmentCodec.psi[ProgramMapSection, Fragment](
      TableId,
      (ext, fragment) => fragment match {
        case pcrPid :: descriptors :: mapping :: HNil => ProgramMapSection(ext, pcrPid, descriptors, mapping)
      },
      pmt => (pmt.extension, pmt.pcrPid :: pmt.programInfoDescriptors :: pmt.componentStreamMapping :: HNil)
    )(fragmentCodec)
}
