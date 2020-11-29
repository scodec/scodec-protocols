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
package v4

import scodec.bits._
import scodec.{ Codec, SizeBound }
import scodec.codecs._
import scodec.stream._

/** Simplified version of the IPv4 header format. */
case class SimpleHeader(
  dataLength: Int,
  id: Int,
  ttl: Int,
  protocol: Int,
  sourceIp: Address,
  destinationIp: Address
)

object SimpleHeader {

  implicit val codec: Codec[SimpleHeader] = {
    val componentCodec = {
      // Word 1 --------------------------------
      ("version"         | constant(bin"0100")) ::
      ("ihl"             | uint4         ) ::
      ("dscp"            | ignore(6)     ) ::
      ("ecn"             | ignore(2)     ) ::
      ("total_length"    | uint16        ) ::
      // Word 2 --------------------------------
      ("id"              | uint16        ) ::
      ("flags"           | ignore(3)     ) ::
      ("fragment_offset" | ignore(13)    ) ::
      // Word 3 --------------------------------
      ("ttl"             | uint8         ) ::
      ("proto"           | uint8         ) ::
      ("checksum"        | bits(16)      ) ::
      // Word 4 --------------------------------
      ("src_ip"          | Codec[Address]) ::
      // Word 5 --------------------------------
      ("dest_ip"         | Codec[Address])
    }.dropUnits

    new Codec[SimpleHeader] {
      def sizeBound = SizeBound.exact(160)

      def encode(header: SimpleHeader) = {
        val totalLength = header.dataLength + 20
        for {
          encoded <- componentCodec.encode(5, totalLength, header.id, header.ttl, header.protocol, BitVector.low(16), header.sourceIp, header.destinationIp)
          chksum = checksum(encoded)
        } yield encoded.patch(80L, chksum)
      }

      def decode(bits: BitVector) = {
        componentCodec.decode(bits) map { _ map {
          t =>
            SimpleHeader(t(1) - 20, t(2), t(3), t(4), t(6), t(7))
        }}
      }
    }
  }

  def sdecoder(ethernetHeader: pcap.EthernetFrameHeader): StreamDecoder[SimpleHeader] =
    if (ethernetHeader.ethertype == Some(pcap.EtherType.IPv4)) StreamDecoder.once(codec)
    else StreamDecoder.empty
}
