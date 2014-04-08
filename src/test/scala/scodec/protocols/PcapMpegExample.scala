package scodec.protocols

import scodec.stream.StreamDecoder
import scodec.stream.decode._
import pcap.{ CaptureFile, LinkType }

/**
 * Example of decoding a PCAP file that contains:
 *  - captured ethernet frames
 *  - of IPv4 packets
 *  - of UDP datagrams
 *  - containing MPEG transport stream packets
 */
object PcapMpegExample extends App {

  case class IpAndPort(address: ip.v4.Address, port: ip.Port)
  case class CapturedPacket(source: IpAndPort, destination: IpAndPort, packet: mpeg.transport.Packet)

  val decoder: StreamDecoder[Timestamped[CapturedPacket]] = CaptureFile.payloadStreamDecoderPF {
    case LinkType.Ethernet =>
      for {
        // Decode an ethernet frame header
        ethernetHeader <- once[pcap.EthernetFrameHeader]
        // Skip this record if the ethertype is not IPv4
        if ethernetHeader.ethertype == Some(0x0800)
        // Decode the IPv4 packet header
        ipHeader <- once[ip.v4.SimpleHeader]
        // Skip this record if the protocol is not UDP
        if ipHeader.protocol == 17
        // Decode the UDP datagram header
        udpDatagram <- once[ip.udp.DatagramHeader]
        // Decode the rest of the data as MPEG transport stream packets
        packets <- tryMany[mpeg.transport.Packet] map { p =>
          CapturedPacket(
            IpAndPort(ipHeader.sourceIp, udpDatagram.sourcePort),
            IpAndPort(ipHeader.destinationIp, udpDatagram.destinationPort),
            p)
        }
      } yield packets
  }

  decoder.decodeMmap(ExampleData.mpegPcapChannel).runLog.run.foreach(println)
}
