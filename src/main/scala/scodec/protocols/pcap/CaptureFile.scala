package scodec.protocols.pcap

import scala.collection.immutable.IndexedSeq
import scodec.Codec
import scodec.codecs._
import shapeless.Iso

case class CaptureFile(
  header: GlobalHeader,
  records: IndexedSeq[Record])

object CaptureFile {
  implicit val iso = Iso.hlist(CaptureFile.apply _, CaptureFile.unapply _)

  implicit val codec: Codec[CaptureFile] = "capture-file" | {
    Codec[GlobalHeader] >>:~ { hdr =>
      implicit val ordering = hdr.ordering
      repeated(Codec[Record]).hlist
  }}.as[CaptureFile]
}
