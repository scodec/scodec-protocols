/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.protocols.mpeg
package transport
package psi

import scodec.Codec
import scodec.bits._
import scodec.codecs._

case class ConditionalAccessTable(
  version: Int,
  current: Boolean,
  descriptors: List[ConditionalAccessDescriptor]
) extends Table {
  def tableId = ConditionalAccessSection.TableId
  def toSections: GroupedSections[ConditionalAccessSection] = ConditionalAccessTable.toSections(this)
}

object ConditionalAccessTable {

  def toSections(cat: ConditionalAccessTable): GroupedSections[ConditionalAccessSection] = {
    val grouped = groupBasedOnSize(cat.descriptors.toVector)
    val lastSection = grouped.size - 1
    val sections = grouped.zipWithIndex.map { case (ds, idx) =>
      ConditionalAccessSection(SectionExtension(65535, cat.version, cat.current, idx, lastSection), ds.toList)
    }
    if (sections.isEmpty)
      GroupedSections(ConditionalAccessSection(SectionExtension(65535, cat.version, cat.current, 0, 0), Nil))
    else
      GroupedSections(sections.head, sections.tail.toList)
  }

  private def groupBasedOnSize(sections: Vector[ConditionalAccessDescriptor]): Vector[Vector[ConditionalAccessDescriptor]] = {
    val MaxBitsLeft = (1024 - 12) * 8L
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

  def fromSections(sections: GroupedSections[ConditionalAccessSection]): Either[String, ConditionalAccessTable] = {
    def extract[A](name: String, f: ConditionalAccessSection => A): Either[String, A] = {
      val extracted = sections.list.map(f).distinct
      if (extracted.size == 1) Right(extracted.head)
      else Left(s"sections have diferring $name: " + extracted.mkString(", "))
    }
    for {
      version <- extract("versions", _.extension.version).right
    } yield {
      val current = sections.list.foldLeft(false) { (acc, s) => acc || s.extension.current }
      ConditionalAccessTable(
        version,
        current,
        (for {
          section <- sections.list
          descriptor <- section.descriptors
        } yield descriptor)
      )
    }
  }

  implicit val tableSupport: TableSupport[ConditionalAccessTable] = new TableSupport[ConditionalAccessTable] {
    def tableId = ConditionalAccessSection.TableId
    def toTable(gs: GroupedSections[Section]) =
      gs.narrow[ConditionalAccessSection].toRight(s"Not CAT sections").right.flatMap { sections => fromSections(sections) }
    def toSections(cat: ConditionalAccessTable) = ConditionalAccessTable.toSections(cat)
  }
}

case class ConditionalAccessSection(
  extension: SectionExtension,
  descriptors: List[ConditionalAccessDescriptor]
) extends ExtendedSection {
  def tableId = ConditionalAccessSection.TableId
}

object ConditionalAccessSection {
  val TableId = 1

  type Fragment = List[ConditionalAccessDescriptor]

  private val fragmentCodec: Codec[Fragment] =
    list(Codec[ConditionalAccessDescriptor])

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
