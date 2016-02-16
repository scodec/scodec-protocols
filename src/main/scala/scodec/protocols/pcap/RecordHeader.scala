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
