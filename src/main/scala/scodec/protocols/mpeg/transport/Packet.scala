package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.Iso

/** Transport stream packet. */
case class Packet(
  header: TransportStreamHeader,
  adaptationField: Option[AdaptationField],
  payload: Option[ByteVector]
)

object Packet {
  implicit def iso = Iso.hlist(Packet.apply _, Packet.unapply _)

  implicit def codec(implicit adaptationField: Codec[AdaptationField]): Codec[Packet] = "packet" | {
    ("header"           | Codec[TransportStreamHeader]                              ) >>:~ { hdr =>
    ("adaptation_field" | conditional(hdr.adaptationFieldIncluded, adaptationField) ) ::
    ("payload"          | conditional(hdr.payloadIncluded, bytes(184))              )
  }}.as[Packet]
}
