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

package scodec.protocols.pcap

import scodec.Codec
import scodec.bits.ByteOrdering

/**
 * Describes the link layer type in a PCAP capture.
 * @see http://www.tcpdump.org/linktypes.html
 */
sealed trait LinkType

/** Companion for [[LinkType]]. */
object LinkType {
  case object Null extends LinkType
  case object Ethernet extends LinkType
  case object Raw extends LinkType
  case object IPv4 extends LinkType
  case object IPv6 extends LinkType
  case object MPEG2TS extends LinkType
  case class Unknown(value: Long) extends LinkType

  def toLong(lt: LinkType): Long = lt match {
    case Null => 0
    case Ethernet => 1
    case Raw => 101
    case IPv4 => 228
    case IPv6 => 229
    case MPEG2TS => 243
    case Unknown(value) => value
  }

  def fromLong(l: Long): LinkType = l match {
    case 0 => Null
    case 1 => Ethernet
    case 101 => Raw
    case 228 => IPv4
    case 229 => IPv6
    case 243 => MPEG2TS
    case other => Unknown(other)
  }

  implicit def codec(implicit bo: ByteOrdering): Codec[LinkType] =
    guint32.xmap[LinkType](fromLong, toLong)
}
