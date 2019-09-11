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
    Codec[GlobalHeader] >>:~ { hdr =>
      vector(Record.codec(hdr.ordering)).hlist
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
    decoderFn <- f(global).fold(e => StreamDecoder.raiseError(CodecError(e)), StreamDecoder.emit)
    recordDecoder =
      RecordHeader.codec(global.ordering) flatMap { header =>
        StreamDecoder.isolate(header.includedLength * 8L) { decoderFn(header) }.strict
      }
    values <- StreamDecoder.many(recordDecoder).flatMap(x => StreamDecoder.emits(x))
  } yield values
}
