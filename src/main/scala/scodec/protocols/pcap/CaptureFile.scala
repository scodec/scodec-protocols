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

import scodec.{ Codec, Err }
import scodec.codecs._
import scodec.stream._

import scodec.protocols.time._

case class CaptureFile(
  header: GlobalHeader,
  records: Vector[Record])

object CaptureFile {
  implicit val codec: Codec[CaptureFile] = "capture-file" | {
    Codec[GlobalHeader].flatPrepend { hdr =>
      vector(Record.codec(hdr.ordering)).tuple
  }}.as[CaptureFile]

  def payloadStreamDecoderPF[A](linkDecoders: PartialFunction[LinkType, StreamDecoder[A]]): StreamDecoder[TimeStamped[A]] =
    payloadStreamDecoder(linkDecoders.lift)

  def payloadStreamDecoder[A](linkDecoders: LinkType => Option[StreamDecoder[A]]): StreamDecoder[TimeStamped[A]] =
    streamDecoder { global =>
      linkDecoders(global.network) match {
        case None => Left(Err(s"unsupported link type ${global.network}"))
        case Some(decoder) => Right {
          hdr => decoder map { value => TimeStamped(hdr.timestamp plusMillis (global.thiszone * 1000L), value) }
        }
      }
    }

  def recordStreamDecoder: StreamDecoder[Record] =
    streamDecoder[Record] { global => Right { hdr =>
      StreamDecoder.once(bits) map { bs =>
        Record(hdr.copy(timestampSeconds = hdr.timestampSeconds + global.thiszone), bs)
      }
    }}

  def streamDecoder[A](f: GlobalHeader => Either[Err, (RecordHeader => StreamDecoder[A])]): StreamDecoder[A] = for {
    global <- StreamDecoder.once(GlobalHeader.codec)
    decoderFn <- f(global).fold(StreamDecoder.raiseError, StreamDecoder.emit)
    recordDecoder =
      RecordHeader.codec(global.ordering) flatMap { header =>
        StreamDecoder.isolate(header.includedLength * 8) { decoderFn(header) }.strict
      }
    values <- StreamDecoder.many(recordDecoder).flatMap(x => StreamDecoder.emits(x))
  } yield values
}
