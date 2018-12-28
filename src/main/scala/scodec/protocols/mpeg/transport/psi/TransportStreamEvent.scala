package scodec.protocols
package mpeg
package transport
package psi

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
              sectionsToTablesForPid.transform(groupingState, section).map(PidStamped(pid, _)).mapResult(state.updated(pid, _))
            case Left(err) =>
              fs2.Chunk(PidStamped(pid, Left(err))).asResult(state)
          }
      }, { state =>
        state.foldLeft(fs2.Chunk.empty: fs2.Chunk[PidStamped[Either[MpegError, TableMessage]], Unit]) { case (acc, (pid, gs)) =>
          acc ++ sectionsToTablesForPid.onComplete(gs).map(PidStamped(pid, _))
        }
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
