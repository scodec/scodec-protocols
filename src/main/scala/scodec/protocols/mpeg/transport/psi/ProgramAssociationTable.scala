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
import scodec.codecs._

case class ProgramAssociationTable(
  tsid: TransportStreamId,
  version: Int,
  current: Boolean,
  programByPid: Map[ProgramNumber, Pid]
) extends Table {
  def tableId = ProgramAssociationSection.TableId
  def toSections: GroupedSections[ProgramAssociationSection] = ProgramAssociationTable.toSections(this)
}

object ProgramAssociationTable {

  val MaxProgramsPerSection = 253

  def toSections(pat: ProgramAssociationTable): GroupedSections[ProgramAssociationSection] = {
    val entries = pat.programByPid.toVector.sortBy { case (ProgramNumber(n), _) => n }
    val groupedEntries = entries.grouped(MaxProgramsPerSection).toVector
    val lastSection = groupedEntries.size - 1
    val sections = groupedEntries.zipWithIndex.map { case (es, idx) =>
      ProgramAssociationSection(SectionExtension(pat.tsid.value, pat.version, pat.current, idx, lastSection), es)
    }
    if (sections.isEmpty)
      GroupedSections(ProgramAssociationSection(SectionExtension(pat.tsid.value, pat.version, pat.current, 0, 0), Vector.empty))
    else
      GroupedSections(sections.head, sections.tail.toList)
  }

  def fromSections(sections: GroupedSections[ProgramAssociationSection]): Either[String, ProgramAssociationTable] = {
    def extract[A](name: String, f: ProgramAssociationSection => A): Either[String, A] = {
      val extracted = sections.list.map(f).distinct
      if (extracted.size == 1) Right(extracted.head)
      else Left(s"sections have diferring $name: " + extracted.mkString(", "))
    }
    for {
      tsid <- extract("TSIDs", _.tsid)
      version <- extract("versions", _.extension.version)
    } yield {
      val current = sections.list.foldLeft(false) { (acc, s) => acc || s.extension.current }
      ProgramAssociationTable(
        tsid,
        version,
        current,
        (for {
          section <- sections.list
          pidMapping <- section.pidMappings
        } yield pidMapping).toMap)
    }
  }

  implicit val tableSupport: TableSupport[ProgramAssociationTable] = new TableSupport[ProgramAssociationTable] {
    def tableId = ProgramAssociationSection.TableId
    def toTable(gs: GroupedSections[Section]) =
      gs.narrow[ProgramAssociationSection].toRight("Not PAT sections").flatMap { sections => fromSections(sections) }
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
      ("program_number" | Codec[ProgramNumber]) ::
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
