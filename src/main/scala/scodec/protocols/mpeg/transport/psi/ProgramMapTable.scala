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

import Descriptor._

case class ProgramMapTable(
  programNumber: ProgramNumber,
  version: Int,
  current: Boolean,
  pcrPid: Pid,
  programInfoDescriptors: List[Descriptor],
  componentStreamMapping: Map[StreamType, List[ProgramMapRecord]]
) extends Table {
  def tableId = ProgramMapSection.TableId
}

object ProgramMapTable {

  def toSection(pmt: ProgramMapTable): ProgramMapSection = {
    ProgramMapSection(
      SectionExtension(pmt.programNumber.value, pmt.version, pmt.current, 0, 0),
      pmt.pcrPid,
      pmt.programInfoDescriptors,
      (for ((st, pmrs) <- pmt.componentStreamMapping.toVector; pmr <- pmrs) yield (st, pmr)).sortBy { case (k, v) => (k.value, v.pid.value) }
    )
  }

  def fromSection(section: ProgramMapSection): ProgramMapTable = {
    val componentStreamMapping = section.componentStreamMapping.foldLeft(Map.empty[StreamType, List[ProgramMapRecord]]) { case (acc, (st, pmr)) =>
      acc.updated(st, acc.get(st).fold(List(pmr))(existing => pmr :: existing))
    }.map { case (k, v) => (k, v.reverse) }
    ProgramMapTable(
      section.programNumber,
      section.extension.version,
      section.extension.current,
      section.pcrPid,
      section.programInfoDescriptors,
      componentStreamMapping
    )
  }

  implicit val tableSupport: TableSupport[ProgramMapTable] = new TableSupport[ProgramMapTable] {
    def tableId = ProgramMapSection.TableId
    def toTable(gs: GroupedSections[Section]) =
      gs.narrow[ProgramMapSection].toRight("Not PMT sections").flatMap { sections =>
        if (sections.tail.isEmpty) Right(fromSection(sections.head))
        else Left(s"PMT supports only 1 section but got ${sections.list.size}")
    }
    def toSections(pmt: ProgramMapTable) = GroupedSections(ProgramMapTable.toSection(pmt))
  }
}

case class StreamType(value: Int)
case class ProgramMapRecord(pid: Pid, descriptors: List[Descriptor])
object ProgramMapRecord {
  def apply(pid: Pid) = new ProgramMapRecord(pid, Nil)
}

case class ProgramMapSection(
  extension: SectionExtension,
  pcrPid: Pid,
  programInfoDescriptors: List[Descriptor],
  componentStreamMapping: Vector[(StreamType, ProgramMapRecord)]
) extends ExtendedSection {
  def tableId = ProgramMapSection.TableId
  def programNumber: ProgramNumber = ProgramNumber(extension.tableIdExtension)
}

object ProgramMapSection {
  val TableId = 2

  private type Fragment = (Pid, List[Descriptor], Vector[(StreamType, ProgramMapRecord)])
  private val fragmentCodec: Codec[Fragment] = {
    def pid: Codec[Pid] = reserved(3) ~> Codec[Pid]
    def descriptors: Codec[List[Descriptor]] =
      reserved(4) ~> variableSizeBytes(uint(12), list(Descriptor.codec))
    def programMapRecord: Codec[ProgramMapRecord] =
      (("pid" | pid) :: ("es_descriptors" | descriptors)).as[ProgramMapRecord]

    ("pcr_pid" | pid) ::
    ("program_info_descriptors" | descriptors) ::
    vector {
      ("stream_type" | uint8.as[StreamType]) :: programMapRecord
    }
  }

  implicit val sectionSubCodec: SectionFragmentCodec[ProgramMapSection] =
    SectionFragmentCodec.psi[ProgramMapSection, Fragment](
      TableId,
      (ext, fragment) => fragment match {
        case (pcrPid, descriptors, mapping) => ProgramMapSection(ext, pcrPid, descriptors, mapping)
      },
      pmt => (pmt.extension, (pmt.pcrPid, pmt.programInfoDescriptors, pmt.componentStreamMapping))
    )(fragmentCodec)
}
