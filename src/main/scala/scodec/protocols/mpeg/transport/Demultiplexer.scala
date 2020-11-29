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
package mpeg
package transport

import scala.language.higherKinds

import fs2._

import scodec.{ Attempt, Codec, DecodeResult, Err }
import scodec.Decoder
import scodec.bits._
import scodec.codecs.fixedSizeBits

import scodec.protocols.mpeg._
import scodec.protocols.mpeg.transport.psi.{ Section, SectionHeader, SectionCodec }

/** Supports depacketization of an MPEG transport stream, represented as a stream of `Packet`s. */
object Demultiplexer {

  sealed trait Result
  case class SectionResult(section: Section) extends Result
  case class PesPacketResult(body: PesPacket) extends Result

  /**
   * Indication that a header was decoded successfully and there was enough information on how to decode the body of the message.
   *
   * Upon receiving a result of this type, the demultiplexer will accumulate the number of bits specified by `neededBits` if that value
   * is defined. If `neededBits` is undefined, the demultiplexer will accumulate all payload bits until the start of the next message (as
   * indicated by the payload unit start indicator). When accumulation has completed, the specified decoder will be invoked to decode
   * a message.
   */
  case class DecodeBody[A](neededBits: Option[Long], decoder: Decoder[A])

  /** Error that indicates any data accumulated by the demultiplexer should be dropped and no further decoding should occur until the next
   * payload start. */
  case class ResetDecodeState(context: List[String]) extends Err {
    def message = "reset decode state"
    def pushContext(ctx: String) = ResetDecodeState(ctx :: context)
  }

  final case class State(byPid: Map[Pid, DecodeState])

  sealed trait DecodeState
  object DecodeState {

    final case class AwaitingHeader(acc: BitVector, startedAtOffsetZero: Boolean) extends DecodeState

    final case class AwaitingBody[A](headerBits: BitVector, neededBits: Option[Long], bitsPostHeader: BitVector, decoder: Decoder[A]) extends DecodeState {
      def decode: Attempt[DecodeResult[A]] = decoder.decode(bitsPostHeader)
      def accumulate(data: BitVector): AwaitingBody[A] = copy(bitsPostHeader = bitsPostHeader ++ data)
    }
  }

  private case class StepResult[+A](state: Option[DecodeState], output: Chunk[Either[DemultiplexerError, A]]) {
    def ++[AA >: A](that: StepResult[AA]): StepResult[AA] = StepResult(that.state, Chunk.concat(List(output, that.output)))
  }
  private object StepResult {
    def noOutput(state: Option[DecodeState]): StepResult[Nothing] = apply(state, Chunk.empty)
    def state(state: DecodeState): StepResult[Nothing] = StepResult(Some(state), Chunk.empty)
    def oneResult[A](state: Option[DecodeState], output: A): StepResult[A] = apply(state, Chunk.singleton(Right(output)))
    def oneError(state: Option[DecodeState], err: DemultiplexerError): StepResult[Nothing] = apply(state, Chunk.singleton(Left(err)))
  }

  /**
   * Stream transducer that converts packets in to sections and PES packets.
   *
   * The packets may span PID values. De-packetization is performed on each PID and as whole messages are received,
   * reassembled messages are emitted.
   *
   * PES packets emitted by this method never include parsed headers -- that is, every emitted PES packet is of
   * type `PesPacket.WithoutHeader`. To get PES packets with parsed headers, use `demultiplexWithPesHeaders`.
   *
   * Errors encountered while depacketizing are emitted.
   *
   * Upon noticing a PID discontinuity, an error is emitted and PID decoding state is discarded, resulting in any in-progress
   * section decoding to be lost for that PID.
   */
  def demultiplex[F[_]](sectionCodec: SectionCodec): Transform.Aux[(Map[Pid, ContinuityCounter], State), Packet, PidStamped[Either[DemultiplexerError, Result]]] =
    demultiplexSectionsAndPesPackets(sectionCodec.decoder, pph => Decoder(b => Attempt.successful(DecodeResult(PesPacket.WithoutHeader(pph.streamId, b), BitVector.empty))))

  /** Variant of `demultiplex` that parses PES packet headers. */
  def demultiplexWithPesHeaders[F[_]](sectionCodec: SectionCodec): Transform.Aux[(Map[Pid, ContinuityCounter], State), Packet, PidStamped[Either[DemultiplexerError, Result]]] =
    demultiplexSectionsAndPesPackets(sectionCodec.decoder, PesPacket.decoder)

  /** Variant of `demultiplex` that allows section and PES decoding to be explicitly specified. */
  def demultiplexSectionsAndPesPackets[F[_]](
    decodeSectionBody: SectionHeader => Decoder[Section],
    decodePesBody: PesPacketHeaderPrefix => Decoder[PesPacket]): Transform.Aux[(Map[Pid, ContinuityCounter], State), Packet, PidStamped[Either[DemultiplexerError, Result]]] = {

    val stuffingByte = bin"11111111"

    def decodeHeader(data: BitVector, startedAtOffsetZero: Boolean): Attempt[DecodeResult[DecodeBody[Result]]] = {
      if (data.sizeLessThan(16)) {
        Attempt.failure(Err.InsufficientBits(16, data.size, Nil))
      } else if (data startsWith stuffingByte) {
        Attempt.failure(ResetDecodeState(Nil))
      } else {
        if (startedAtOffsetZero && data.take(16) == hex"0001".bits) {
          if (data.sizeLessThan(40)) {
            Attempt.failure(Err.InsufficientBits(40, data.size, Nil))
          } else {
            Codec[PesPacketHeaderPrefix].decode(data.drop(16)) map { _ map { header =>
                val neededBits = if (header.length == 0) None else Some(header.length * 8L)
                DecodeBody(neededBits, decodePesBody(header).map(PesPacketResult.apply))
            }}
          }
        } else {
          if (data.sizeLessThan(24)) {
            Attempt.failure(Err.InsufficientBits(24, data.size, Nil))
          } else {
            Codec[SectionHeader].decode(data) map { _ map { header =>
              DecodeBody(Some(header.length * 8L), decodeSectionBody(header).map(SectionResult.apply))
            }}
          }
        }
      }
    }
    demultiplexGeneral(decodeHeader)
  }

  /**
   * Most general way to perform demultiplexing, allowing parsing of arbitrary headers and decoding of a specified output type.
   *
   * When processing the payload in a packet, the start of the payload is passed along to `decodeHeader`, which determines how to
   * process the body of the message.
   *
   * In addition to the payload data, a flag is passed to `decodeHeader` -- true is passed when the payload data started at byte 0 of
   * the packet and false is passed when the payload data started later in the packet.
   *
   * See the documentation on `DecodeBody` for more information.
   */
  def demultiplexGeneral[F[_], Out](
    decodeHeader: (BitVector, Boolean) => Attempt[DecodeResult[DecodeBody[Out]]]
  ): Transform.Aux[(Map[Pid, ContinuityCounter], State), Packet, PidStamped[Either[DemultiplexerError, Out]]] = {

    def processBody[A](awaitingBody: DecodeState.AwaitingBody[A], payloadUnitStartAfterData: Boolean): StepResult[Out] = {
      val haveFullBody = awaitingBody.neededBits match {
        case None => payloadUnitStartAfterData
        case Some(needed) => awaitingBody.bitsPostHeader.size >= needed
      }
      if (haveFullBody) {
        awaitingBody.decode match {
          case Attempt.Successful(DecodeResult(body, remainder)) =>
            val decoded = StepResult.oneResult(None, body.asInstanceOf[Out]) // Safe cast b/c DecodeBody must provide a Decoder[Out]
            decoded ++ processHeader(remainder, false, payloadUnitStartAfterData)
          case Attempt.Failure(err) =>
            val out = {
              if (err.isInstanceOf[ResetDecodeState]) Chunk.empty
              else Chunk.singleton(Left(DemultiplexerError.Decoding(
                awaitingBody.headerBits ++
                  awaitingBody.neededBits.
                    map { n => awaitingBody.bitsPostHeader.take(n) }.
                    getOrElse(awaitingBody.bitsPostHeader), err)))
            }
            val failure = StepResult(None, out)
            awaitingBody.neededBits match {
              case Some(n) =>
                val remainder = awaitingBody.bitsPostHeader.drop(n.toLong)
                failure ++ processHeader(remainder, false, payloadUnitStartAfterData)
              case None => failure
            }
        }
      } else {
        StepResult.state(awaitingBody)
      }
    }

    def processHeader(acc: BitVector, startedAtOffsetZero: Boolean, payloadUnitStartAfterData: Boolean): StepResult[Out] = {
      decodeHeader(acc, startedAtOffsetZero) match {
        case Attempt.Failure(e: Err.InsufficientBits) =>
          StepResult.state(DecodeState.AwaitingHeader(acc, startedAtOffsetZero))
        case Attempt.Failure(_: ResetDecodeState) =>
          StepResult.noOutput(None)
        case Attempt.Failure(e) =>
          StepResult.oneError(None, DemultiplexerError.Decoding(acc, e))
        case Attempt.Successful(DecodeResult(DecodeBody(neededBits, decoder), bitsPostHeader)) =>
          val guardedDecoder = neededBits match {
            case None => decoder
            case Some(n) => fixedSizeBits(n, decoder.decodeOnly)
          }
          processBody(DecodeState.AwaitingBody(acc.take(24L), neededBits, bitsPostHeader, guardedDecoder), payloadUnitStartAfterData)
      }
    }

    def resume(state: DecodeState, newData: BitVector, payloadUnitStartAfterData: Boolean): StepResult[Out] = state match {
      case ah: DecodeState.AwaitingHeader =>
        processHeader(ah.acc ++ newData, ah.startedAtOffsetZero, payloadUnitStartAfterData)

      case ab: DecodeState.AwaitingBody[_] =>
        processBody(ab.accumulate(newData), payloadUnitStartAfterData)
    }

    def handlePacket(state: Option[DecodeState], packet: Packet): StepResult[Out] = {
      packet.payload match {
        case None => StepResult.noOutput(state)
        case Some(payload) =>
          val currentResult = state match {
            case None => StepResult.noOutput(state)
            case Some(state) =>
              val currentData = packet.payloadUnitStart.map { start => payload.take(start.toLong * 8L) }.getOrElse(payload)
              resume(state, currentData, payloadUnitStartAfterData = packet.payloadUnitStart.isDefined)
          }
          packet.payloadUnitStart match {
            case None =>
              currentResult
            case Some(start) =>
              val nextResult = processHeader(payload.drop(start * 8L), start == 0, false)
              currentResult ++ nextResult
          }
      }
    }

    val demux = Transform.stateful[State, Either[PidStamped[DemultiplexerError.Discontinuity], Packet], PidStamped[Either[DemultiplexerError, Out]]](State(Map.empty)) { (state, event) =>
      event match {
        case Right(packet) =>
          val pid = packet.header.pid
          val oldStateForPid = state.byPid.get(pid)
          val result = handlePacket(oldStateForPid, packet)
          val newState = State(result.state.map { s => state.byPid.updated(pid, s) }.getOrElse(state.byPid - pid))
          val out = result.output.map { e => PidStamped(pid, e) }
          (newState, out)
        case Left(discontinuity) =>
          val newState = State(state.byPid - discontinuity.pid)
          val out = Chunk.singleton(PidStamped(discontinuity.pid, Left(discontinuity.value)))
          (newState, out)
      }
    }

    Packet.validateContinuity andThen demux
  }
}
