package scodec.protocols
package ip
package tcp

import scodec.Codec
import scodec.codecs._
import scodec.stream._

case class TcpFlags(cwr: Boolean, ecn: Boolean, urg: Boolean, ack: Boolean, psh: Boolean, rst: Boolean, syn: Boolean, fin: Boolean)
object TcpFlags {
  implicit val codec: Codec[TcpFlags] = {
    ("cwr" | bool(1)) ::
    ("ecn" | bool(1)) ::
    ("urg" | bool(1)) ::
    ("ack" | bool(1)) ::
    ("psh" | bool(1)) ::
    ("rst" | bool(1)) ::
    ("syn" | bool(1)) ::
    ("fin" | bool(1))
  }.as[TcpFlags]
}

case class TcpHeader(
  sourcePort: Port,
  destinationPort: Port,
  sequenceNumber: Long,
  ackNumber: Long,
  dataOffset: Int,
  flags: TcpFlags,
  windowSize: Int,
  checksum: Int,
  urgentPointer: Int,
  options: Vector[Long])

object TcpHeader {
  val port = Codec[Port]
  implicit val codec: Codec[TcpHeader] = {
    ("source port"      | port)            ::
    ("destination port" | port)            ::
    ("seqNumber"        | uint32)          ::
    ("ackNumber"        | uint32)          ::
    (("dataOffset"      | uint4)           >>:~ { headerWords =>
    ("reserved"         | ignore(4))       ::
    ("flags"            | Codec[TcpFlags]) ::
    ("windowSize"       | uint16)          ::
    ("checksum"         | uint16)          ::
    ("urgentPointer"    | uint16)          ::
    ("options"          | vectorOfN(provide(headerWords - 5), uint32))
  } ) }.dropUnits.as[TcpHeader]

  def sdecoder(protocol: Int): StreamDecoder[TcpHeader] =
    if (protocol == ip.Protocols.Tcp) decode.once[TcpHeader]
    else decode.empty
}
