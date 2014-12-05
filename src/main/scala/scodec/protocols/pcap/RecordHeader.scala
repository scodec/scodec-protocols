package scodec.protocols.pcap

import scala.concurrent.duration._
import scodec.bits.ByteOrdering
import scodec.Codec
import scodec.codecs._

import org.joda.time.DateTime

case class RecordHeader(
  timestampSeconds: Long,
  timestampMicros: Long,
  includedLength: Long,
  originalLength: Long) {
  def timestamp: DateTime = RecordHeader.timestamp(timestampSeconds, timestampMicros)
  def fullPayload: Boolean = includedLength == originalLength
}

object RecordHeader {

  private def timestamp(seconds: Long, micros: Long): DateTime = new DateTime((seconds.seconds + micros.micros).toMillis)

  def apply(time: DateTime, includedLength: Long, originalLength: Long): RecordHeader =
    RecordHeader(time.getMillis / 1000, time.getMillisOfSecond * 1000, includedLength, originalLength)

  implicit def codec(implicit ordering: ByteOrdering): Codec[RecordHeader] = "record-header" | {
    ("ts_sec"   | guint32 ) ::
    ("ts_usec"  | guint32 ) ::
    ("incl_len" | guint32 ) ::
    ("orig_len" | guint32 )
  }.as[RecordHeader]
}
