package scodec.protocols

import scodec.stream.StreamDecoder
import scodec.stream.decode
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

  val decoder: StreamDecoder[TimeStamped[CapturedPacket]] = CaptureFile.payloadStreamDecoderPF {
    case LinkType.Ethernet =>
      for {
        ethernetHeader <- pcap.EthernetFrameHeader.sdecoder
        ipHeader <- ip.v4.SimpleHeader.sdecoder(ethernetHeader)
        udpDatagram <- ip.udp.DatagramHeader.sdecoder(ipHeader.protocol)
        packets <- decode.tryMany[mpeg.transport.Packet] map { p =>
          CapturedPacket(
            IpAndPort(ipHeader.sourceIp, udpDatagram.sourcePort),
            IpAndPort(ipHeader.destinationIp, udpDatagram.destinationPort),
            p)
        }
      } yield packets
  }

  decoder.decodeMmap(ExampleData.mpegPcapChannel).runLog.run.foreach(println)
}
