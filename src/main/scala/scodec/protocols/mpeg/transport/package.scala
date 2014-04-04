package scodec.protocols
package mpeg

import scodec.stream.decode._
import scodec.protocols.pcap.{ CaptureFile, LinkType }

package object transport {

  val timestampedPacketStreamDecoder: StreamDecoder[Timestamped[Packet]] =
    CaptureFile.payloadStreamDecoderPF {
      case LinkType.Ethernet => advance((22 + 20) * 8) ++ tryMany[Packet]
      case LinkType.MPEG2TS => tryMany[Packet]
    }
}
