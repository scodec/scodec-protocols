package scodec.protocols.mpeg
package transport

import scalaz.std.AllInstances._
import scodec.Codec
import scodec.codecs._
import shapeless.Iso

case class TransportStreamHeader(
  transportErrorIndicator: Boolean,
  payloadUnitStartIndicator: Boolean,
  transportPriority: Boolean,
  pid: Pid,
  scramblingControl: Int,
  adaptationFieldControl: Int,
  continuityCounter: Int
) {
  def adaptationFieldIncluded: Boolean = adaptationFieldControl >= 2
  def payloadIncluded: Boolean = adaptationFieldControl == 1 || adaptationFieldControl == 3
}

object TransportStreamHeader {
  implicit def iso = Iso.hlist(TransportStreamHeader.apply _, TransportStreamHeader.unapply _)

  implicit val codec: Codec[TransportStreamHeader] = "transport_stream_header" | fixedSizeBytes(4,
    ("syncByte"                  | constant(0x47) ) :~>:
    ("transportErrorIndicator"   | bool           ) ::
    ("payloadUnitStartIndicator" | bool           ) ::
    ("transportPriority"         | bool           ) ::
    ("pid"                       | Codec[Pid]     ) ::
    ("scramblingControl"         | uint2          ) ::
    ("adaptationFieldControl"    | uint2          ) ::
    ("continuityCounter"         | uint4          )
  ).as[TransportStreamHeader]
}
