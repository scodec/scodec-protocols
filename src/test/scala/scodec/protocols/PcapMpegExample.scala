package scodec.protocols

import cats.effect.IO
import scodec.protocols.time.TimeStamped
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

  def mpegPcapChannel: java.nio.channels.FileChannel = ???

  val decoder: StreamDecoder[TimeStamped[CapturedPacket]] = CaptureFile.payloadStreamDecoderPF(chunkSize = 256) {
    case LinkType.Ethernet =>
      for {
        ethernetHeader <- pcap.EthernetFrameHeader.sdecoder
        ipHeader <- ip.v4.SimpleHeader.sdecoder(ethernetHeader)
        udpDatagram <- ip.udp.DatagramHeader.sdecoder(ipHeader.protocol)
        packets <- decode.tryManyChunked[mpeg.transport.Packet](chunkSize = 10) map { p =>
          CapturedPacket(
            IpAndPort(ipHeader.sourceIp, udpDatagram.sourcePort),
            IpAndPort(ipHeader.destinationIp, udpDatagram.destinationPort),
            p)
        }
      } yield packets
  }

  decoder.decodeMmap[IO](mpegPcapChannel).runLog.unsafeRunSync.foreach(println)
}
