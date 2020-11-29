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
package pcap

import scodec.Codec
import scodec.codecs._
import scodec.stream._

/**
 * Header of an ethernet frame as captured in a pcap file.
 */
case class EthernetFrameHeader(
  destination: MacAddress,
  source: MacAddress,
  ethertypeOrLength: Int
) {
  def length: Option[Int] = if (ethertypeOrLength <= 1500) Some(ethertypeOrLength) else None
  def ethertype: Option[Int] = if (ethertypeOrLength > 1500) Some(ethertypeOrLength) else None
}

object EthernetFrameHeader {
  implicit val codec: Codec[EthernetFrameHeader] = {
    val macAddress = Codec[MacAddress]
    ("destination" | macAddress) ::
    ("source"      | macAddress) ::
    ("ethertype"   | uint16)
  }.as[EthernetFrameHeader]

  val sdecoder: StreamDecoder[EthernetFrameHeader] = StreamDecoder.once(codec)
}

object EtherType {
  val IPv4 = 0x0800
  val IPv6 = 0x86DD
  val VLAN = 0x8100
}

