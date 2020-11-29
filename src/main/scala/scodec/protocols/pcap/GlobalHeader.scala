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

import scodec.Err
import scodec.bits.{ BitVector, ByteOrdering }
import scodec.{ Attempt, Codec, DecodeResult, SizeBound }
import scodec.codecs._

case class GlobalHeader(
  ordering: ByteOrdering,
  versionMajor: Int,
  versionMinor: Int,
  thiszone: Int,
  sigfigs: Long,
  snaplen: Long,
  network: LinkType)

object GlobalHeader {
  private val MagicNumber = 0xa1b2c3d4L
  private val MagicNumberRev = 0xd4c3b2a1L

  private val byteOrdering: Codec[ByteOrdering] = new Codec[ByteOrdering] {
    def sizeBound = SizeBound.exact(32)

    def encode(bo: ByteOrdering) =
      endiannessDependent(uint32, uint32L)(bo).encode(MagicNumber)

    def decode(buf: BitVector) =
      uint32.decode(buf).flatMap {
        case DecodeResult(MagicNumber, rest) => Attempt.successful(DecodeResult(ByteOrdering.BigEndian, rest))
        case DecodeResult(MagicNumberRev, rest) => Attempt.successful(DecodeResult(ByteOrdering.LittleEndian, rest))
        case DecodeResult(other, rest) => Attempt.failure(Err(s"unable to detect byte ordering due to unrecognized magic number $other"))
      }

    override def toString = "byteOrdering"
  }

  implicit val codec: Codec[GlobalHeader] = "global-header" | {
    ("magic_number"  | byteOrdering    ).flatPrepend { implicit ordering =>
    ("version_major" | guint16         ) ::
    ("version_minor" | guint16         ) ::
    ("thiszone"      | gint32          ) ::
    ("sigfigs"       | guint32         ) ::
    ("snaplen"       | guint32         ) ::
    ("network"       | LinkType.codec  )
  }}.as[GlobalHeader]
}
