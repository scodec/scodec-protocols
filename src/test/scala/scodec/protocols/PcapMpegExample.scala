package scodec.protocols

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream
import scodec.protocols.time.TimeStamped
import scodec.stream.StreamDecoder
import pcap.{ CaptureFile, LinkType }
import java.nio.file.Paths

/**
 * Example of decoding a PCAP file that contains:
 *  - captured ethernet frames
 *  - of IPv4 packets
 *  - of UDP datagrams
 *  - containing MPEG transport stream packets
 */
object PcapMpegExample extends IOApp {

  case class IpAndPort(address: ip.v4.Address, port: ip.Port)
  case class CapturedPacket(source: IpAndPort, destination: IpAndPort, packet: mpeg.transport.Packet)

  val decoder: StreamDecoder[TimeStamped[CapturedPacket]] = CaptureFile.payloadStreamDecoderPF {
    case LinkType.Ethernet =>
      for {
        ethernetHeader <- pcap.EthernetFrameHeader.sdecoder
        ipHeader <- ip.v4.SimpleHeader.sdecoder(ethernetHeader)
        udpDatagram <- ip.udp.DatagramHeader.sdecoder(ipHeader.protocol)
        packets <- StreamDecoder.tryMany(mpeg.transport.Packet.codec).map { p =>
          CapturedPacket(
            IpAndPort(ipHeader.sourceIp, udpDatagram.sourcePort),
            IpAndPort(ipHeader.destinationIp, udpDatagram.destinationPort),
            p)
        }
      } yield packets
  }

  override def run(args: List[String]): IO[ExitCode] = {
    if (args.size == 1) {
      Stream.eval(IO(Paths.get(args.head))).flatMap { mpegPcapPath =>
        Stream.resource(Blocker[IO]).flatMap { blocker =>
          fs2.io.file.readAll[IO](mpegPcapPath, blocker, 4096).through(decoder.toPipeByte).map(_.toString).showLinesStdOutAsync(blocker)
        }
      }.compile.drain.as(ExitCode.Success)
    } else {
      IO(println("Must pass a path to a PCAP file")).as(ExitCode.Error)
    }
  }
}
