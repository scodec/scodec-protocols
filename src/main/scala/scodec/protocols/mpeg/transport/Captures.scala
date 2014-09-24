package scodec.protocols
package mpeg
package transport

import scodec.stream.decode._
import scodec.protocols.pcap.{ CaptureFile, LinkType }

import scalaz.syntax.bind._

object Captures {

  val timestampedPacketStreamDecoder: StreamDecoder[TimeStamped[Packet]] =
    CaptureFile.payloadStreamDecoderPF {
      case LinkType.Ethernet =>
        (pcap.EthernetFrameHeader.sdecoder >>= ip.v4.SimpleHeader.sdecoder >>= ip.udp.DatagramHeader.sdecoder) >> tryManyChunked[Packet](7)
      case LinkType.MPEG2TS => tryManyChunked[Packet](512)
    }

}
