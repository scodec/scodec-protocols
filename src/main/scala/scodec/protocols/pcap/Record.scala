package scodec.protocols.pcap

import scodec.bits.{ BitVector, ByteOrdering }
import scodec.Codec
import scodec.codecs._

case class Record(
  header: RecordHeader,
  data: BitVector)

object Record {
  implicit def codec(implicit ordering: ByteOrdering): Codec[Record] = "record" | {
    ("record_header" | Codec[RecordHeader]               ) >>:~ { hdr =>
    ("record_data"   | bits(hdr.includedLength.toInt * 8L) ).hlist
  }}.as[Record]
}
