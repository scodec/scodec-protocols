package scodec.protocols
package mpeg
package transport
package psi

import scalaz.\/.{ right, left }
import scalaz.stream.Process

class SectionCodecTest extends ProtocolsSpec {

  "the SectionCodec class" should {

    "support decoding a stream of packets in to a stream of sections" which {

      val sectionCodec = SectionCodec.supporting[ProgramAssociationSection]

      "handles case where section starts at beginning of packet and is fully contained within packet" in {
        val pas = ProgramAssociationTable.toSections(ProgramAssociationTable(TransportStreamId(1), 15, true, Map(ProgramNumber(1) -> Pid(2)))).head
        val pasEnc = sectionCodec.encodeValid(pas)
        val packet = Packet.payload(Pid(0), ContinuityCounter(0), Some(0), pasEnc)

        val p = Process.emit(packet).toSource pipe sectionCodec.depacketize
        p.runLog.run shouldBe IndexedSeq(pas).map(s => PidStamped(Pid(0), right(s)))
      }

      "handles case where section starts at beginning of packet and spans multiple packets" in {
        val pas = ProgramAssociationTable.toSections(ProgramAssociationTable(TransportStreamId(1), 15, true,
          (for (i <- 0 until ProgramAssociationTable.MaxProgramsPerSection)
          yield ProgramNumber(i) -> Pid(i)).toMap
        )).head
        val pasEnc = sectionCodec.encodeValid(pas)
        val packets = Packet.packetize(Pid(0), ContinuityCounter(0), pasEnc)

        val p = Process.emitAll(packets).toSource pipe sectionCodec.depacketize
        p.runLog.run shouldBe IndexedSeq(pas).map(s => PidStamped(Pid(0), right(s)))
      }

      "checks packet continuity" in {
        val pas = ProgramAssociationTable.toSections(ProgramAssociationTable(TransportStreamId(1), 15, true,
          (for (i <- 0 until ProgramAssociationTable.MaxProgramsPerSection)
          yield ProgramNumber(i) -> Pid(i)).toMap
        )).head
        val pasEnc = sectionCodec.encodeValid(pas)
        val packets = Packet.packetize(Pid(0), ContinuityCounter(1), pasEnc)
        val withDiscontinuity = packets.updated(0, packets.head.copy(header = packets.head.header.copy(continuityCounter = ContinuityCounter(15))))

        val p = Process.emitAll(withDiscontinuity).toSource pipe sectionCodec.depacketize
        p.runLog.run shouldBe IndexedSeq(PidStamped(Pid(0), left(DepacketizationError.Discontinuity(ContinuityCounter(15), ContinuityCounter(2)))))
      }

    }
  }

}
