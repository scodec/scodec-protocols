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
package ip
package udp

import scodec.bits.BitVector
import scodec.Codec
import scodec.codecs._

case class Datagram(sourcePort: Port, destinationPort: Port, data: BitVector)

object Datagram {
  implicit val codec: Codec[Datagram] = new Codec[Datagram] {
    def sizeBound = Codec[DatagramHeader].sizeBound.atLeast

    def encode(dg: Datagram) = for {
      encHeader <- Codec.encode(DatagramHeader(dg.sourcePort, dg.destinationPort, 8 + dg.data.bytes.size.toInt, 0))
      chksum = checksum(encHeader ++ dg.data)
    } yield encHeader.dropRight(16) ++ chksum ++ dg.data

    def decode(b: BitVector) = (for {
      header <- Codec[DatagramHeader]
      data <- bits(8L * (header.length - 8))
    } yield Datagram(header.sourcePort, header.destinationPort, data)).decode(b)
  }
}
