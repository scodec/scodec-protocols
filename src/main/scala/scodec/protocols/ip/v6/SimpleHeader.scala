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
package v6

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.stream._

/** Simple version of an IPv6 header. Does not support extension headers. */
case class SimpleHeader(
  trafficClass: Int,
  flowLabel: Int,
  payloadLength: Int,
  protocol: Int,
  hopLimit: Int,
  sourceIp: Address,
  destinationIp: Address
)

object SimpleHeader {
  implicit val codec: Codec[SimpleHeader] = {
    ("version"             | constant(bin"0110")) ~>
    ("traffic_class"       | uint8) ::
    ("flow_label"          | uint(20)) ::
    ("payload_length"      | uint(16)) ::
    ("next_header"         | uint8) ::
    ("hop_limit"           | uint8) ::
    ("source_address"      | Codec[Address]) ::
    ("destination_address" | Codec[Address])
  }.as[SimpleHeader]

  def sdecoder(ethernetHeader: pcap.EthernetFrameHeader): StreamDecoder[SimpleHeader] =
    if (ethernetHeader.ethertype == Some(pcap.EtherType.IPv6)) StreamDecoder.once(codec)
    else StreamDecoder.empty
}
