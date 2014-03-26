package scodec.protocols.mpeg
package transport

import scala.collection.immutable.IndexedSeq

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.Iso

/** Transport stream packet. */
case class Packet(
  header: TransportStreamHeader,
  adaptationField: Option[AdaptationField],
  payloadUnitStart: Option[Int],
  payload: Option[BitVector]
)

object Packet {

  def packetize(pid: Pid, startingCountinuityCounter: Int, section: BitVector): IndexedSeq[Packet] = {
    @annotation.tailrec
    def go(first: Boolean, cc: Int, remaining: BitVector, acc: IndexedSeq[Packet]): IndexedSeq[Packet] = {
      if (remaining.isEmpty) acc
      else {
        val (packetData, remData) = remaining.splitAt(8 * (if (first) 183 else 184))
        go(false, (cc + 1) % 16, remData, acc :+ payload(pid, cc, if (first) Some(0) else None, packetData))
      }
    }
    go(true, startingCountinuityCounter, section, IndexedSeq.empty)
  }

  def payload(pid: Pid, continuityCounter: Int, payloadUnitStart: Option[Int], payload: BitVector): Packet = {
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

  implicit def iso = Iso.hlist(Packet.apply _, Packet.unapply _)

  implicit def codec(implicit adaptationField: Codec[AdaptationField]): Codec[Packet] =
    "packet" | fixedSizeBytes(188,
      ("header"           | Codec[TransportStreamHeader]                              ) >>:~ { hdr =>
      ("adaptation_field" | conditional(hdr.adaptationFieldIncluded, adaptationField) ) ::
      ("adaptation_field" | conditional(hdr.payloadUnitStartIndicator, uint8)         ) ::
      ("payload"          | conditional(hdr.payloadIncluded, bits)                    )
    }).as[Packet]
}
