package scodec.protocols
package pcap

import scalaz.\/
import scalaz.\/.{ left, right }
import scalaz.syntax.std.option._
import scodec.{ Codec, Decoder, Err }
import scodec.bits.BitVector
import scodec.codecs._
import scodec.stream._

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
        case None => left(Err(s"unsupported link type ${global.network}"))
        case Some(decoder) => right {
          hdr => decoder map { value => TimeStamped(hdr.timestamp plus (global.thiszone * 1000), value) }
        }
      }
    }

  def recordStreamDecoder: StreamDecoder[Record] =
    streamDecoder[Record] { global => right { hdr =>
      decode.once(bits) map { bs =>
        Record(hdr.copy(timestampSeconds = hdr.timestampSeconds + global.thiszone), bs)
      }
    }}

  def streamDecoder[A](f: GlobalHeader => Err \/ (RecordHeader => StreamDecoder[A])): StreamDecoder[A] = for {
    global <- decode.once[GlobalHeader]
    decoderFn <- f(global).fold(decode.fail, decode.emit)
    values <- decode.many(RecordHeader.codec(global.ordering)) flatMap { header =>
      decode.isolate(header.includedLength * 8) { decoderFn(header) }
    }
  } yield values
}
