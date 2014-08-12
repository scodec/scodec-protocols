package scodec.protocols.mpeg
package transport

import scala.collection.immutable.IndexedSeq

import scalaz.\/
import \/.{ left, right }
import scalaz.stream.{ Process, Process1 }
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

/** Transport stream packet. */
case class Packet(
  header: TransportStreamHeader,
  adaptationField: Option[AdaptationField],
  payloadUnitStart: Option[Int],
  payload: Option[BitVector]
)

object Packet {

  def packetize(pid: Pid, startingCountinuityCounter: ContinuityCounter, section: BitVector): IndexedSeq[Packet] = {
    @annotation.tailrec
    def go(first: Boolean, cc: ContinuityCounter, remaining: BitVector, acc: IndexedSeq[Packet]): IndexedSeq[Packet] = {
      if (remaining.isEmpty) acc
      else {
        val (packetData, remData) = remaining.splitAt(8 * (if (first) 183 else 184))
        go(false, cc.next, remData, acc :+ payload(pid, cc, if (first) Some(0) else None, packetData))
      }
    }
    go(true, startingCountinuityCounter, section, IndexedSeq.empty)
  }

  def payload(pid: Pid, continuityCounter: ContinuityCounter, payloadUnitStart: Option[Int], payload: BitVector): Packet = {
    val thisPid = pid
    val thisContinuityCounter = continuityCounter
    val thisPayloadUnitStart = payloadUnitStart
    val payloadLength = 8 * (if (payloadUnitStart.isDefined) 183 else 184)
    require(payload.length <= payloadLength, s"payload too long; must be <= $payloadLength")
    val thisPayload = payload ++ BitVector.high(payloadLength - payload.length)
    Packet(
      header = TransportStreamHeader(
        transportErrorIndicator = false,
        payloadUnitStartIndicator = payloadUnitStart.isDefined,
        transportPriority = false,
        pid = thisPid,
        scramblingControl = 0,
        adaptationFieldControl = 1,
        continuityCounter = thisContinuityCounter
      ),
      adaptationField = None,
      payloadUnitStart = thisPayloadUnitStart,
      payload = Some(thisPayload))
  }

  implicit def codec(implicit adaptationField: Codec[AdaptationField]): Codec[Packet] =
    "packet" | fixedSizeBytes(188,
      ("header"           | Codec[TransportStreamHeader]                              ) >>:~ { hdr =>
      ("adaptation_field" | conditional(hdr.adaptationFieldIncluded, adaptationField) ) ::
      ("adaptation_field" | conditional(hdr.payloadUnitStartIndicator, uint8)         ) ::
      ("payload"          | conditional(hdr.payloadIncluded, bits)                    )
    }).as[Packet]

  def validateContinuity: Process1[Packet, DepacketizationError.Discontinuity \/ Packet] = {
    def go(state: Map[Pid, ContinuityCounter]): Process1[Packet, DepacketizationError.Discontinuity \/ Packet] = {
      Process.await1[Packet] flatMap { packet =>
        val pid = packet.header.pid
        val currentContinuityCounter = packet.header.continuityCounter
        state.get(pid).map { lastContinuityCounter =>
          if (lastContinuityCounter.next == currentContinuityCounter)
            Process.halt
          else
            Process.emit(left(DepacketizationError.Discontinuity(pid, lastContinuityCounter, currentContinuityCounter)))
        }.getOrElse(Process.halt) ++ Process.emit(right(packet)) ++ go(state + (pid -> currentContinuityCounter))
      }
    }
    go(Map.empty)
  }
}
