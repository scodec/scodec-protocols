package scodec.protocols
package pcap

import scalaz.syntax.std.boolean._
import scodec.Codec
import scodec.codecs._
import shapeless.Iso

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
  implicit val iso = Iso.hlist(EthernetFrameHeader.apply _, EthernetFrameHeader.unapply _)

  implicit val codec: Codec[EthernetFrameHeader] = {
    val macAddress = Codec[MacAddress]
    ("destination" | macAddress) ::
    ("source"      | macAddress) ::
    ("ethertype"   | uint16)
  }.as[EthernetFrameHeader]
}
