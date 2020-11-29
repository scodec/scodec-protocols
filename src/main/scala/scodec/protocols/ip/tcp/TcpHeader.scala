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
package tcp

import scodec.Codec
import scodec.codecs._
import scodec.stream._

case class TcpFlags(cwr: Boolean, ecn: Boolean, urg: Boolean, ack: Boolean, psh: Boolean, rst: Boolean, syn: Boolean, fin: Boolean)
object TcpFlags {
  implicit val codec: Codec[TcpFlags] = {
    ("cwr" | bool(1)) ::
    ("ecn" | bool(1)) ::
    ("urg" | bool(1)) ::
    ("ack" | bool(1)) ::
    ("psh" | bool(1)) ::
    ("rst" | bool(1)) ::
    ("syn" | bool(1)) ::
    ("fin" | bool(1))
  }.as[TcpFlags]
}

case class TcpHeader(
  sourcePort: Port,
  destinationPort: Port,
  sequenceNumber: Long,
  ackNumber: Long,
  dataOffset: Int,
  flags: TcpFlags,
  windowSize: Int,
  checksum: Int,
  urgentPointer: Int,
  options: Vector[Long])

object TcpHeader {
  val port = Codec[Port]
  implicit val codec: Codec[TcpHeader] = {
    ("source port"      | port)            ::
    ("destination port" | port)            ::
    ("seqNumber"        | uint32)          ::
    ("ackNumber"        | uint32)          ::
    (("dataOffset"      | uint4).flatPrepend { headerWords =>
    ("reserved"         | ignore(4))       ::
    ("flags"            | Codec[TcpFlags]) ::
    ("windowSize"       | uint16)          ::
    ("checksum"         | uint16)          ::
    ("urgentPointer"    | uint16)          ::
    ("options"          | vectorOfN(provide(headerWords - 5), uint32))
  } ) }.dropUnits.as[TcpHeader]

  def sdecoder(protocol: Int): StreamDecoder[TcpHeader] =
    if (protocol == ip.Protocols.Tcp) StreamDecoder.once(codec)
    else StreamDecoder.empty
}
