package scodec.protocols
package pcap

import scalaz.\/
import scalaz.\/.{ left, right }
import scalaz.syntax.std.option._
import scodec.{ Codec, Decoder, Err }
import scodec.bits.BitVector
import scodec.codecs._
import scodec.stream._

import scalaz.stream._
import scalaz.concurrent._
import scodec.{ Codec, Attempt, DecodeResult }
import scodec.stream.decode.DecodingError

import shapeless.Lazy

case class CaptureFile(
  header: GlobalHeader,
  records: Vector[Record])

object CaptureFile {
  implicit val codec: Codec[CaptureFile] = "capture-file" | {
    Codec[GlobalHeader] >>:~ { hdr =>
      vector(Record.codec(hdr.ordering)).hlist
  }}.as[CaptureFile]

  def payloadStreamDecoderPF[A](chunkSize: Int = 256)(linkDecoders: PartialFunction[LinkType, StreamDecoder[A]]): StreamDecoder[TimeStamped[A]] =
    payloadStreamDecoder(chunkSize)(linkDecoders.lift)

  def payloadStreamDecoder[A](chunkSize: Int = 256)(linkDecoders: LinkType => Option[StreamDecoder[A]]): StreamDecoder[TimeStamped[A]] =
    streamDecoder(chunkSize) { global =>
      linkDecoders(global.network) match {
        case None => left(Err(s"unsupported link type ${global.network}"))
        case Some(decoder) => right {
          hdr => decoder map { value => TimeStamped(hdr.timestamp plus (global.thiszone * 1000), value) }
        }
      }
    }

  def recordStreamDecoder(chunkSize: Int = 256): StreamDecoder[Record] =
    streamDecoder[Record](chunkSize) { global => right { hdr =>
      decode.once(bits) map { bs =>
        Record(hdr.copy(timestampSeconds = hdr.timestampSeconds + global.thiszone), bs)
      }
    }}

  def streamDecoder[A](chunkSize: Int = 256)(f: GlobalHeader => Err \/ (RecordHeader => StreamDecoder[A])): StreamDecoder[A] = for {
    global <- decode.once[GlobalHeader]
    decoderFn <- f(global).fold(decode.fail, decode.emit)
    recordDecoder =
      RecordHeader.codec(global.ordering) flatMap { header =>
        decode.isolateBytes(header.includedLength) { decoderFn(header) }.strict
      }
    values <- decode.manyChunked(chunkSize)(Lazy(recordDecoder)).flatMap(x => decode.emitAll(x))
  } yield values
}
