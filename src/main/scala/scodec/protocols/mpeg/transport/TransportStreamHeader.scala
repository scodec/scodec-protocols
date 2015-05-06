package scodec.protocols.mpeg
package transport

import scodec.Codec
import scodec.codecs._

case class TransportStreamHeader(
  transportErrorIndicator: Boolean,
  payloadUnitStartIndicator: Boolean,
  transportPriority: Boolean,
  pid: Pid,
  scramblingControl: Int,
  adaptationFieldControl: Int,
  continuityCounter: ContinuityCounter
) {
  def adaptationFieldIncluded: Boolean = adaptationFieldControl >= 2
  def payloadIncluded: Boolean = adaptationFieldControl == 1 || adaptationFieldControl == 3
}

object TransportStreamHeader {
  implicit val codec: Codec[TransportStreamHeader] = "transport_stream_header" | fixedSizeBytes(4,
    ("syncByte"                  | constant(0x47)          ) :~>:
    ("transportErrorIndicator"   | bool                    ) ::
    ("payloadUnitStartIndicator" | bool                    ) ::
    ("transportPriority"         | bool                    ) ::
    ("pid"                       | Codec[Pid]              ) ::
    ("scramblingControl"         | uint2                   ) ::
    ("adaptationFieldControl"    | uint2                   ) ::
    ("continuityCounter"         | Codec[ContinuityCounter])
  ).as[TransportStreamHeader]
}
