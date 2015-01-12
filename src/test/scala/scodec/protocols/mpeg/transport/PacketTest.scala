package scodec.protocols
package mpeg
package transport
package psi

import scalaz.std.AllInstances._
import scalaz.syntax.id._
import scalaz.syntax.foldable._

import scodec.bits._
import scodec.interop.scalaz._

class PacketTest extends ProtocolsSpec {

  "the Packet class" should {

    "support packetizing multiple sections in to a single packet" in {
      val a = ByteVector.fill(10)(0).bits
      val b = ByteVector.fill(10)(1).bits
      val c = ByteVector.fill(10)(2).bits
      val sections = Vector(a, b, c)
      val packets = Packet.packetizeMany(Pid(0), ContinuityCounter(0), sections)
      packets shouldBe Vector(Packet.payload(Pid(0), ContinuityCounter(0), Some(0), a ++ b ++ c ++ BitVector.fill((183 * 8) - a.size - b.size - c.size)(true)))
    }

    "support packetizing multiple sections across multiple packets" in {
      val sections = (0 until 256).map { x => ByteVector.fill(10)(x).bits }.toVector
      val data = sections.concatenate
      val packets = Packet.packetizeMany(Pid(0), ContinuityCounter(0), sections)

      packets.zipWithIndex.foreach { case (packet, idx) =>
        val payloadOffset = if (idx == 0) 0 else 10 * ((idx * 183) / 10 + 1) - (idx * 183)
        val offset = 183 * 8 * idx
        packets(idx) shouldBe Packet.payload(Pid(0), ContinuityCounter(idx), Some(payloadOffset), data.drop(offset).take(183 * 8))
      }
    }
  }
}
