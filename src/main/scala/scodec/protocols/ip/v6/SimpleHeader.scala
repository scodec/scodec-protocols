package scodec.protocols
package ip
package v6

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.literals._
import scodec.stream._

/** Simple version of an IPv6 header. Does not support extension headers. */
case class SimpleHeader(
  trafficClass: Int,
  flowLabel: Int,
  payloadLength: Int,
  protocol: Int,
  hopLimit: Int,
  sourceIp: Address,
  destinationIp: Address
)

object SimpleHeader {
  implicit val codec: Codec[SimpleHeader] = {
    ("version"             | bin"0110") :~>:
    ("traffic_class"       | uint8) ::
    ("flow_label"          | uint(20)) ::
    ("payload_length"      | uint(16)) ::
    ("next_header"         | uint8) ::
    ("hop_limit"           | uint8) ::
    ("source_address"      | Codec[Address]) ::
    ("destination_address" | Codec[Address])
  }.as[SimpleHeader]

  def sdecoder(ethernetHeader: pcap.EthernetFrameHeader): StreamDecoder[SimpleHeader] =
    if (ethernetHeader.ethertype == Some(pcap.EtherType.IPv6)) StreamDecoder.once(codec)
    else StreamDecoder.empty
}
