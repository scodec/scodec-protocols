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

package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs._

case class TransportStreamHeader(
  transportErrorIndicator: Boolean,
  payloadUnitStartIndicator: Boolean,
  transportPriority: Boolean,
  pid: Pid,
  scramblingControl: Int,
  adaptationFieldControl: Int,
  continuityCounter: ContinuityCounter
) {
  def adaptationFieldIncluded: Boolean = adaptationFieldControl >= 2
  def payloadIncluded: Boolean = adaptationFieldControl == 1 || adaptationFieldControl == 3
}

object TransportStreamHeader {
  implicit val codec: Codec[TransportStreamHeader] = "transport_stream_header" | fixedSizeBytes(4,
    ("syncByte"                  | constant(0x47)          ) ~>
    ("transportErrorIndicator"   | bool                    ) ::
    ("payloadUnitStartIndicator" | bool                    ) ::
    ("transportPriority"         | bool                    ) ::
    ("pid"                       | Codec[Pid]              ) ::
    ("scramblingControl"         | uint2                   ) ::
    ("adaptationFieldControl"    | uint2                   ) ::
    ("continuityCounter"         | Codec[ContinuityCounter])
  ).as[TransportStreamHeader]
}
