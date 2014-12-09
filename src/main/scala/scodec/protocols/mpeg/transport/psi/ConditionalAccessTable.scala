package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, NonEmptyList, Tag, Tags }
import scalaz.\/.{ left, right }
import scalaz.std.AllInstances._
import scalaz.syntax.foldable._
import scalaz.syntax.std.option._
import scodec.Codec
import scodec.bits._
import scodec.codecs._
import shapeless._

case class ConditionalAccessTable(
  version: Int,
  current: Boolean,
  descriptors: Vector[ConditionalAccessDescriptor]
) extends Table {
  def tableId = ConditionalAccessSection.TableId
  def toSections: NonEmptyList[ConditionalAccessSection] = ConditionalAccessTable.toSections(this)
}

object ConditionalAccessTable {

  def toSections(cat: ConditionalAccessTable): NonEmptyList[ConditionalAccessSection] = {
    val grouped = groupBasedOnSize(cat.descriptors)
    val lastSection = grouped.size - 1
    val sections = grouped.zipWithIndex.map { case (ds, idx) =>
      ConditionalAccessSection(SectionExtension(65535, cat.version, cat.current, idx, lastSection), ds)
    }
    if (sections.isEmpty)
      NonEmptyList(ConditionalAccessSection(SectionExtension(65535, cat.version, cat.current, 0, 0), Vector.empty))
    else
      NonEmptyList(sections.head, sections.tail: _*)
  }

  private def groupBasedOnSize(sections: Vector[ConditionalAccessDescriptor]): Vector[Vector[ConditionalAccessDescriptor]] = {
    val MaxBitsLeft = (1024 - 12) * 8
    def sizeOf(c: ConditionalAccessDescriptor): Long = (6 * 8) + c.privateData.size
    @annotation.tailrec
    def go(remaining: Vector[ConditionalAccessDescriptor], cur: Vector[ConditionalAccessDescriptor], bitsLeft: Long, acc: Vector[Vector[ConditionalAccessDescriptor]]): Vector[Vector[ConditionalAccessDescriptor]] = {
      if (remaining.isEmpty) acc :+ cur
      else {
        val next = remaining.head
        val bitsNeeded = (6 * 8) + sizeOf(next)
        val newBitsLeft = bitsLeft - bitsNeeded
        if (newBitsLeft >= 0) go(remaining.tail, cur :+ next, newBitsLeft, acc)
        else {
          go(remaining, Vector.empty, MaxBitsLeft, acc :+ cur)
        }
      }
    }
    go(sections, Vector.empty, MaxBitsLeft, Vector.empty)
  }

  def fromSections(sections: NonEmptyList[ConditionalAccessSection]): String \/ ConditionalAccessTable = {
    def extract[A](name: String, f: ConditionalAccessSection => A): String \/ A = {
      val extracted = sections.map(f).list.distinct
      if (extracted.size == 1) right(extracted.head)
      else left(s"sections have diferring $name: " + extracted.mkString(", "))
    }
    for {
      version <- extract("versions", _.extension.version)
      current = Tag.unwrap(sections.foldMap { p => Tags.Disjunction(p.extension.current) })
    } yield ConditionalAccessTable(
      version,
      current,
      sections.foldMap { _.descriptors }
    )
  }

  implicit val tableSupport: TableSupport[ConditionalAccessTable] = new TableSupport[ConditionalAccessTable] {
    def tableId = ConditionalAccessSection.TableId
    def toTable(gs: GroupedSections) =
      gs.as[ConditionalAccessSection].toRightDisjunction(s"Not CAT sections").flatMap { sections => fromSections(sections) }
    def toSections(cat: ConditionalAccessTable) = ConditionalAccessTable.toSections(cat)
  }
}

case class ConditionalAccessSection(
  extension: SectionExtension,
  descriptors: Vector[ConditionalAccessDescriptor]
) extends ExtendedSection {
  def tableId = ConditionalAccessSection.TableId
}

object ConditionalAccessSection {
  val TableId = 1

  type Fragment = Vector[ConditionalAccessDescriptor]

  private val fragmentCodec: Codec[Fragment] =
    vector(Codec[ConditionalAccessDescriptor])

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
