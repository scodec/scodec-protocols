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

import scodec.bits._

class PacketTest extends ProtocolsSpec {

  test("support packetizing multiple sections in to a single packet") {
    val a = ByteVector.fill(10)(0).bits
    val b = ByteVector.fill(10)(1).bits
    val c = ByteVector.fill(10)(2).bits
    val sections = Vector(a, b, c)
    val packets = Packet.packetizeMany(Pid(0), ContinuityCounter(0), sections)
    assertEquals(packets, Vector(Packet.payload(Pid(0), ContinuityCounter(0), Some(0), a ++ b ++ c ++ BitVector.fill((183 * 8) - a.size - b.size - c.size)(true))))
  }

  test("support packetizing multiple sections across multiple packets") {
    val sections = (0 until 256).map { x => ByteVector.fill(10)(x).bits }.toVector
    val data = sections.foldLeft(BitVector.empty)(_ ++ _)
    val packets = Packet.packetizeMany(Pid(0), ContinuityCounter(0), sections)

    packets.zipWithIndex.foreach { case (packet, idx) =>
      val payloadOffset = if (idx == 0) 0 else 10 * ((idx * 183) / 10 + 1) - (idx * 183)
      val offset = 183L * 8 * idx
      assertEquals(packets(idx), Packet.payload(Pid(0), ContinuityCounter(idx), Some(payloadOffset), data.drop(offset).take(183 * 8)))
    }
  }
}
