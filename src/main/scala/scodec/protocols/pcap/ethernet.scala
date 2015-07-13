package scodec.protocols
package pcap

import scalaz.syntax.std.boolean._
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
  def length: Option[Int] = (ethertypeOrLength <= 1500).option(ethertypeOrLength)
  def ethertype: Option[Int] = (ethertypeOrLength > 1500).option(ethertypeOrLength)
}

object EthernetFrameHeader {
  implicit val codec: Codec[EthernetFrameHeader] = {
    val macAddress = Codec[MacAddress]
    ("destination" | macAddress) ::
    ("source"      | macAddress) ::
    ("ethertype"   | uint16)
  }.as[EthernetFrameHeader]

  val sdecoder: StreamDecoder[EthernetFrameHeader] = decode.once[pcap.EthernetFrameHeader]
}

object EtherType {
  val IPv4 = 0x0800
  val IPv6 = 0x86DD
  val VLAN = 0x8100
}

