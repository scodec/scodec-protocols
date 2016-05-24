package scodec.protocols
package pcap

import scodec.{ Codec, Err }
import scodec.codecs._
import scodec.stream._

import scodec.protocols.time._

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
        case None => Left(Err(s"unsupported link type ${global.network}"))
        case Some(decoder) => Right {
          hdr => decoder map { value => TimeStamped(hdr.timestamp plusMillis (global.thiszone * 1000L), value) }
        }
      }
    }

  def recordStreamDecoder(chunkSize: Int = 256): StreamDecoder[Record] =
    streamDecoder[Record](chunkSize) { global => Right { hdr =>
      decode.once(bits) map { bs =>
        Record(hdr.copy(timestampSeconds = hdr.timestampSeconds + global.thiszone), bs)
      }
    }}

  def streamDecoder[A](chunkSize: Int = 256)(f: GlobalHeader => Either[Err, (RecordHeader => StreamDecoder[A])]): StreamDecoder[A] = for {
    global <- decode.once[GlobalHeader]
    decoderFn <- f(global).fold(decode.fail, decode.emit)
    recordDecoder =
      RecordHeader.codec(global.ordering) flatMap { header =>
        decode.isolateBytes(header.includedLength) { decoderFn(header) }.strict
      }
    values <- decode.manyChunked(chunkSize)(Lazy(recordDecoder)).flatMap(x => decode.emits(x))
  } yield values
}
