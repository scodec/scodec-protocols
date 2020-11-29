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

import java.time.Instant
import java.time.temporal.ChronoField

import scala.concurrent.duration._
import scodec.bits.ByteOrdering
import scodec.Codec
import scodec.codecs._

case class RecordHeader(
  timestampSeconds: Long,
  timestampMicros: Long,
  includedLength: Long,
  originalLength: Long) {
  def timestamp: Instant = RecordHeader.timestamp(timestampSeconds, timestampMicros)
  def fullPayload: Boolean = includedLength == originalLength
}

object RecordHeader {

  private def timestamp(seconds: Long, micros: Long): Instant = Instant.ofEpochMilli((seconds.seconds + micros.micros).toMillis)

  def apply(time: Instant, includedLength: Long, originalLength: Long): RecordHeader =
    RecordHeader(time.getEpochSecond, time.get(ChronoField.MILLI_OF_SECOND) * 1000L, includedLength, originalLength)

  implicit def codec(implicit ordering: ByteOrdering): Codec[RecordHeader] = "record-header" | {
    ("ts_sec"   | guint32 ) ::
    ("ts_usec"  | guint32 ) ::
    ("incl_len" | guint32 ) ::
    ("orig_len" | guint32 )
  }.as[RecordHeader]
}
