package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/-, -\/, NonEmptyList }
import scalaz.\/.{ right, left }
import scalaz.stream.Process

class GroupingTest extends ProtocolsSpec {

  "the GroupedSections class" should {

    "support grouping a stream of extended sections in to a stream of grouped sections" which {

      val des = gropuExtendedSections[ProgramAssociationSection]

      val pat3: ProgramAssociationTable = {
        val pidMap = (0 until ProgramAssociationTable.MaxProgramsPerSection * 3).map { n => ProgramNumber(n) -> Pid(n) }.toMap
        ProgramAssociationTable(TransportStreamId(5), 1, true, pidMap)
      }

      "handles stream of a specific table id and extension" in {
        val p = Process.emitAll(pat3.toSections.list).toSource pipe des map {
          case \/-(sections) => ProgramAssociationTable.fromSections(sections)
          case l @ -\/(_) => l
        }
        p.runLog.run shouldBe IndexedSeq(right(pat3))
      }

      "handles stream containing sections for the same table id but differing extension ids" in {
        val patA = pat3
        val patB = pat3.copy(tsid = TransportStreamId(pat3.tsid.value + 1), programByPid = pat3.programByPid.map { case (prg, Pid(n)) => prg -> Pid(n + 1)} )

        val sections = Process.emitAll(patA.toSections.list) interleave Process.emitAll(patB.toSections.list)
        val p = sections.toSource pipe des map { _ flatMap ProgramAssociationTable.fromSections }
        p.runLog.run shouldBe IndexedSeq(right(patA), right(patB))
      }
    }
  }
}
