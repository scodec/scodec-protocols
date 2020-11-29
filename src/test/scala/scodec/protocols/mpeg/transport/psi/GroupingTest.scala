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

package scodec.protocols
package mpeg
package transport
package psi

import fs2._

class GroupingTest extends ProtocolsSpec {

  val des = GroupedSections.groupExtendedSections[ProgramAssociationSection].toPipe[Pure]

  val pat3: ProgramAssociationTable = {
    val pidMap = (0 until ProgramAssociationTable.MaxProgramsPerSection * 3).map { n => ProgramNumber(n) -> Pid(n) }.toMap
    ProgramAssociationTable(TransportStreamId(5), 1, true, pidMap)
  }

  test("handles stream of a specific table id and extension") {
    val p = Stream.emits(pat3.toSections.list).through(des).map {
      case Right(sections) => ProgramAssociationTable.fromSections(sections)
      case l @ Left(_) => l
    }
    assertEquals(p.toList, List(Right(pat3)))
  }

  test("handles stream containing sections for the same table id but differing extension ids") {
    val patA = pat3
    val patB = pat3.copy(tsid = TransportStreamId(pat3.tsid.value + 1), programByPid = pat3.programByPid.map { case (prg, Pid(n)) => prg -> Pid(n + 1)} )

    val sections = Stream.emits(patA.toSections.list) interleave Stream.emits(patB.toSections.list)
    val p = sections.through(des).map { _.right.flatMap(ProgramAssociationTable.fromSections) }
    assertEquals(p.toList, List(Right(patA), Right(patB)))
  }
}
