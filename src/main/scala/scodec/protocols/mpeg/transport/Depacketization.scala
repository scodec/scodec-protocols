package scodec.protocols.mpeg
package transport

import scalaz.{ \/, -\/, \/- }
import \/.{ left, right }
import scalaz.stream.{ Process1, Process }

import scodec.{ Attempt, Codec, DecodeResult, DecodingContext, Err, SizeBound }
import scodec.bits._
import scodec.stream.decode.{ StreamDecoder, many => decodeMany }

import psi.{ Section, SectionHeader, SectionCodec }

/** Supports depacketization of an MPEG transport stream, represented as a stream of `Packet`s. */
object Depacketization {

  sealed trait Result
  case class SectionResult(section: Section) extends Result
  case class PesPacketResult(body: PesPacket) extends Result

  private sealed trait DecodeState
  private case class AwaitingHeader(acc: BitVector) extends DecodeState
  private case class AwaitingSectionBody(header: SectionHeader, acc: BitVector) extends DecodeState
  private case class AwaitingPesBody(header: PesPacketHeaderPrefix, acc: BitVector) extends DecodeState

  /**
   * Stream transducer that converts packets in to sections and PES packets.
   *
   * The packets may span PID values. De-packetization is performed on each PID and as whole messages are received,
   * reassembled messages are emitted.
   *
   * PES packets emitted by this method never include parsed headers -- that is, every emitted PES packet is of
   * type `PesPacket.WithoutHeader`. To get PES packets with parsed headers, use `depacketizeWithPesHeaders`.
   *
   * Errors encountered while depacketizing are emitted.
   *
   * Upon noticing a PID discontinuity, an error is emitted and PID decoding state is discarded, resulting in any in-progress
   * section decoding to be lost for that PID.
   */
  def depacketize(sectionCodec: SectionCodec): Process1[Packet, PidStamped[DepacketizationError \/ Result]] =
    depacketizeGeneral(sectionCodec.decodeSection(_)(_), (pph, b) => Attempt.successful(DecodeResult(PesPacket.WithoutHeader(pph.streamId, b), BitVector.empty)))

  /** Variant of `depacketize` that parses PES packet headers. */
  def depacketizeWithPesHeaders(sectionCodec: SectionCodec): Process1[Packet, PidStamped[DepacketizationError \/ Result]] =
    depacketizeGeneral(sectionCodec.decodeSection(_)(_), PesPacket.decode)

  /** Generic variant of `depacketize` that allows section and PES decoding to be explicitly specified. */
  def depacketizeGeneral(
    decodeSectionBody: (SectionHeader, BitVector) => Attempt[DecodeResult[Section]],
    decodePesBody: (PesPacketHeaderPrefix, BitVector) => Attempt[DecodeResult[PesPacket]]
  ): Process1[Packet, PidStamped[DepacketizationError \/ Result]] = {

    type Step = Process1[PidStamped[DepacketizationError.Discontinuity] \/ Packet, PidStamped[DepacketizationError \/ Result]]

    def pidSpecificErr(pid: Pid, e: DepacketizationError): PidStamped[DepacketizationError \/ Result] =
      PidStamped(pid, left(e))

    def pidSpecificSection(pid: Pid, s: Section): PidStamped[DepacketizationError \/ Result] =
      PidStamped(pid, right(SectionResult(s)))

    def pidSpecificPesPacket(pid: Pid, pesPacket: PesPacket): PidStamped[DepacketizationError \/ Result] =
      PidStamped(pid, right(PesPacketResult(pesPacket)))

    def nextMessage(state: Map[Pid, DecodeState], pid: Pid, payloadUnitStart: Option[Int], payload: BitVector): Step = {
      payloadUnitStart match {
        case None => go(state)
        case Some(start) =>
          val bits = payload.drop(start * 8L)
          if (bits.sizeLessThan(16)) {
            go(state + (pid -> AwaitingHeader(bits)))
          } else {
            // Check for PES start code prefix
            if (start == 0 && bits.take(16) == hex"0001".bits) {
              if (bits.sizeLessThan(40)) {
                go(state + (pid -> AwaitingHeader(bits)))
              } else {
                Codec[PesPacketHeaderPrefix].decode(bits.drop(16)) match {
                  case Attempt.Successful(DecodeResult(header, bitsPostHeader)) =>
                    pesBody(state, pid, header, bitsPostHeader, None)
                  case Attempt.Failure(err) =>
                    Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ go(state - pid)
                }
              }
            } else {
              if (bits.sizeLessThan(32)) {
                go(state + (pid -> AwaitingHeader(bits)))
              } else {
                Codec[SectionHeader].decode(bits) match {
                  case Attempt.Failure(err) =>
                    Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ go(state - pid)
                  case Attempt.Successful(DecodeResult(header, bitsPostHeader)) =>
                    sectionBody(state, pid, header, bitsPostHeader)
                }
              }
            }
          }
      }
    }

    def pesBody(state: Map[Pid, DecodeState], pid: Pid, header: PesPacketHeaderPrefix, bitsPostHeader: BitVector, packet: Option[Packet]): Step = {
      def doDecodePesBody(pesBodyBits: BitVector): Step = {
        decodePesBody(header, pesBodyBits) match {
          case Attempt.Successful(DecodeResult(pesBody, rest)) =>
            Process.emit(pidSpecificPesPacket(pid, pesBody)) ++ go(state - pid)
          case Attempt.Failure(err) =>
            Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ go(state - pid)
        }
      }

      // TODO if header.length is 0, must decode until next packet with payload start indicator
      if (header.length == 0L) {
        if (packet.map { _.payloadUnitStart.isDefined }.getOrElse(false)) {
          doDecodePesBody(bitsPostHeader) ++ handlePacket(state - pid, packet.get)
        } else {
          go(state + (pid -> AwaitingPesBody(header, bitsPostHeader)))
        }
      } else {
        val neededBits = header.length * 8
        if (bitsPostHeader.size < neededBits) {
          go(state + (pid -> AwaitingPesBody(header, bitsPostHeader)))
        } else {
          doDecodePesBody(bitsPostHeader.take(neededBits.toLong)) ++ go(state - pid)
        }
      }
    }

    def sectionBody(state: Map[Pid, DecodeState], pid: Pid, header: SectionHeader, bitsPostHeader: BitVector): Step = {
      val neededBits = header.length * 8
      if (bitsPostHeader.size < neededBits) {
        go(state + (pid -> AwaitingSectionBody(header, bitsPostHeader)))
      } else {
        decodeSectionBody(header, bitsPostHeader) match {
          case Attempt.Failure(err) =>
            val rest = bitsPostHeader.drop(neededBits.toLong)
            Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ potentiallyNextSection(state, pid, rest)
          case Attempt.Successful(DecodeResult(section, rest)) =>
            Process.emit(pidSpecificSection(pid, section)) ++ potentiallyNextSection(state, pid, rest)
        }
      }
    }

    def potentiallyNextSection(state: Map[Pid, DecodeState], pid: Pid, payload: BitVector): Step = {
      // Peek at table_id -- if we see 0xff, then there are no further sections in this packet
      if (payload.size >= 8 && payload.take(8) != BitVector.high(8))
        nextMessage(state - pid, pid, Some(0), payload)
      else go(state - pid)
    }

    def handlePacket(state: Map[Pid, DecodeState], packet: Packet): Step = {
      val pid = packet.header.pid
      packet.payload match {
        case None => go(state)
        case Some(payload) =>
          state.get(packet.header.pid) match {
            case None =>
              nextMessage(state, packet.header.pid, packet.payloadUnitStart, payload)
            case Some(AwaitingHeader(acc)) =>
              nextMessage(state, packet.header.pid, Some(0), acc ++ payload)
            case Some(AwaitingSectionBody(header, acc)) =>
              sectionBody(state, packet.header.pid, header, acc ++ payload)
            case Some(AwaitingPesBody(header, acc)) =>
              pesBody(state, packet.header.pid, header, acc ++ payload, Some(packet))
          }
      }
    }

    def go(state: Map[Pid, DecodeState]): Step =
      Process.await1[PidStamped[DepacketizationError.Discontinuity] \/ Packet].flatMap {
        case -\/(discontinuity) =>
          Process.emit(pidSpecificErr(discontinuity.pid, discontinuity.value)) ++ go(state - discontinuity.pid)

        case \/-(packet) =>
          handlePacket(state, packet)
      }

    Packet.validateContinuity pipe go(Map.empty)
  }

  /** Provides a stream decoder that decodes a bitstream of 188 byte MPEG packets in to a stream of messages. */
  def packetStreamDecoder(sectionCodec: SectionCodec): StreamDecoder[PidStamped[DepacketizationError \/ Result]] = decodeMany[Packet] pipe depacketize(sectionCodec)
}
