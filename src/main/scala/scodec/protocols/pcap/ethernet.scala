package scodec.protocols
package pcap

import scodec.Codec
import scodec.codecs._
import scodec.stream._

/**
 * Header of an ethernet frame as captured in a pcap file.
 */
case class EthernetFrameHeader(
  destination: MacAddress,
  source: MacAddress,
  ethertypeOrLength: Int
) {
  def length: Option[Int] = if (ethertypeOrLength <= 1500) Some(ethertypeOrLength) else None
  def ethertype: Option[Int] = if (ethertypeOrLength > 1500) Some(ethertypeOrLength) else None
}

object EthernetFrameHeader {
  implicit val codec: Codec[EthernetFrameHeader] = {
    val macAddress = Codec[MacAddress]
    ("destination" | macAddress) ::
    ("source"      | macAddress) ::
    ("ethertype"   | uint16)
  }.as[EthernetFrameHeader]

  val sdecoder: StreamDecoder[EthernetFrameHeader] = StreamDecoder.once(codec)
}

object EtherType {
  val IPv4 = 0x0800
  val IPv6 = 0x86DD
  val VLAN = 0x8100
}

