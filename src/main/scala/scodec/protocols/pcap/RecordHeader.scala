package scodec.protocols.pcap

import scala.concurrent.duration._
import scodec.bits.ByteOrdering
import scodec.Codec
import scodec.codecs._

case class RecordHeader(
  timestampSeconds: Long,
  timestampMicros: Long,
  includedLength: Long,
  originalLength: Long) {
  def timestamp: Double = RecordHeader.timestamp(timestampSeconds, timestampMicros)
  def fullPayload: Boolean = includedLength == originalLength
}

object RecordHeader {

  private val oneSecondInMicros = 1.second.toMicros.toDouble
  private def timestamp(seconds: Long, micros: Long): Double = seconds + (micros / oneSecondInMicros)

  implicit def codec(implicit ordering: ByteOrdering): Codec[RecordHeader] = "record-header" | {
    ("ts_sec"   | guint32 ) ::
    ("ts_usec"  | guint32 ) ::
    ("incl_len" | guint32 ) ::
    ("orig_len" | guint32 )
  }.as[RecordHeader]
}
