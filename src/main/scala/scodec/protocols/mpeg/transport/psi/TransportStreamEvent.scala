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
package psi

import cats.data.Chain
import fs2.Chunk
import scodec.bits.BitVector

import psi.{ Table => TableMessage }

abstract class TransportStreamEvent

object TransportStreamEvent {
  case class Pes(pid: Pid, pes: PesPacket) extends TransportStreamEvent
  case class Table(pid: Pid, table: TableMessage) extends TransportStreamEvent
  case class ScrambledPayload(pid: Pid, payload: BitVector) extends TransportStreamEvent
  case class Metadata[A](pid: Option[Pid], metadata: A) extends TransportStreamEvent
  case class Error(pid: Option[Pid], err: MpegError) extends TransportStreamEvent

  def pes(pid: Pid, pes: PesPacket): TransportStreamEvent = Pes(pid, pes)
  def table(pid: Pid, table: TableMessage): TransportStreamEvent = Table(pid, table)
  def scrambledPayload(pid: Pid, content: BitVector): TransportStreamEvent = ScrambledPayload(pid, content)
  def metadata[A](md: A): TransportStreamEvent = Metadata(None, md)
  def metadata[A](pid: Pid, md: A): TransportStreamEvent = Metadata(Some(pid), md)
  def error(pid: Pid, e: MpegError): TransportStreamEvent = Error(Some(pid), e)
  def error(pid: Option[Pid], e: MpegError): TransportStreamEvent = Error(pid, e)

  private def sectionsToTables(
    group: Transform[Section, Either[GroupingError, GroupedSections[Section]]], tableBuilder: TableBuilder
  ): Transform[PidStamped[Either[MpegError, Section]], TransportStreamEvent] = {

    import MpegError._

    val sectionsToTablesForPid: Transform.Aux[group.S, Section, Either[MpegError, TableMessage]] =
      group.map {
        case Left(e) => Left(e)
        case Right(gs) => tableBuilder.build(gs)
      }

    val sectionsToTables: Transform.Aux[Map[Pid, group.S], PidStamped[Either[MpegError, Section]], PidStamped[Either[MpegError, TableMessage]]] =
      Transform(Map.empty[Pid, group.S])({
        case (state, PidStamped(pid, e)) =>
          e match {
            case Right(section) =>
              val groupingState = state.getOrElse(pid, group.initial)
              val (s, out) = sectionsToTablesForPid.transform(groupingState, section)
              (state.updated(pid, s), out.map(PidStamped(pid, _)))
            case Left(err) =>
              (state, Chunk.singleton(PidStamped(pid, Left(err))))
          }
      }, { state =>
        Chunk.concat(state.foldLeft(Chain.empty[Chunk[PidStamped[Either[MpegError, TableMessage]]]]) { case (acc, (pid, gs)) =>
          acc :+ sectionsToTablesForPid.onComplete(gs).map(PidStamped(pid, _))
        }.toList)
      })

    sectionsToTables.andThen(PidStamped.preserve(passErrors(TransportStreamIndex.build))).map { case PidStamped(pid, value) =>
      value match {
        case Left(e) => error(pid, e)
        case Right(Left(tsi)) => metadata(tsi)
        case Right(Right(tbl)) => table(pid, tbl)
      }
    }
  }

  def fromPacketStream(
    sectionCodec: SectionCodec,
    group: Transform[Section, Either[GroupingError, GroupedSections[Section]]],
    tableBuilder: TableBuilder
  ): Transform[Packet, TransportStreamEvent] = {
    val demuxed: Transform[Packet, TransportStreamEvent] = {
      Demultiplexer.demultiplex(sectionCodec).andThen(
        sectionsToTables(group, tableBuilder).semipass[PidStamped[Either[DemultiplexerError, Demultiplexer.Result]], TransportStreamEvent](
          {
            case PidStamped(pid, Right(Demultiplexer.SectionResult(section))) => Right(PidStamped(pid, Right(section)))
            case PidStamped(pid, Right(Demultiplexer.PesPacketResult(p))) => Left(pes(pid, p))
            case PidStamped(pid, Left(e)) => Right(PidStamped(pid, Left(e.toMpegError)))
          }))
    }
    demuxed.semipass[Packet, TransportStreamEvent]({
      case Packet(header, _, _, Some(payload)) if header.scramblingControl != 0 =>
        Left(scrambledPayload(header.pid, payload))
      case p @ Packet(_, _, _, _) =>
        Right(p)
    })
  }

  def fromSectionStream(
    group: Transform[Section, Either[GroupingError, GroupedSections[Section]]],
    tableBuilder: TableBuilder
  ): Transform[PidStamped[Section], TransportStreamEvent] =
    sectionsToTables(group, tableBuilder).contramap[PidStamped[Section]](_.map(Right(_)))
}
