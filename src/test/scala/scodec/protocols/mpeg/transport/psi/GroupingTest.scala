package scodec.protocols
package mpeg
package transport
package psi

import fs2._

class GroupingTest extends ProtocolsSpec {

  "the GroupedSections class" should {

    "support grouping a stream of extended sections in to a stream of grouped sections" which {

      val des = GroupedSections.groupExtendedSections[ProgramAssociationSection].toPipe[Pure]

      val pat3: ProgramAssociationTable = {
        val pidMap = (0 until ProgramAssociationTable.MaxProgramsPerSection * 3).map { n => ProgramNumber(n) -> Pid(n) }.toMap
        ProgramAssociationTable(TransportStreamId(5), 1, true, pidMap)
      }

      "handles stream of a specific table id and extension" in {
        val p = Stream.emits(pat3.toSections.list).through(des).map {
          case Right(sections) => ProgramAssociationTable.fromSections(sections)
          case l @ Left(_) => l
        }
        p.toList shouldBe List(Right(pat3))
      }

      "handles stream containing sections for the same table id but differing extension ids" in {
        val patA = pat3
        val patB = pat3.copy(tsid = TransportStreamId(pat3.tsid.value + 1), programByPid = pat3.programByPid.map { case (prg, Pid(n)) => prg -> Pid(n + 1)} )

        val sections = Stream.emits(patA.toSections.list) interleave Stream.emits(patB.toSections.list)
        val p = sections.through(des).map { _.right.flatMap(ProgramAssociationTable.fromSections) }
        p.toList shouldBe List(Right(patA), Right(patB))
      }
    }
  }
}
