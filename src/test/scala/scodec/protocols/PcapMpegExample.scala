/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.protocols

import cats.effect.{IO, IOApp}
import scodec.protocols.time.TimeStamped
import scodec.stream.StreamDecoder
import pcap.{ CaptureFile, LinkType }

/**
 * Example of decoding a PCAP file that contains:
 *  - captured ethernet frames
 *  - of IPv4 packets
 *  - of UDP datagrams
 *  - containing MPEG transport stream packets
 */
object PcapMpegExample extends IOApp.Simple {

  case class IpAndPort(address: ip.v4.Address, port: ip.Port)
  case class CapturedPacket(source: IpAndPort, destination: IpAndPort, packet: mpeg.transport.Packet)

  val run: IO[Unit] = {
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

    val capture = java.nio.file.Paths.get("path/to/pcap")
    fs2.io.file.Files[IO].readAll(capture, 4096)
      .through(decoder.toPipeByte)
      .map(_.toString)
      .showLinesStdOut
      .compile
      .drain
  }
}
