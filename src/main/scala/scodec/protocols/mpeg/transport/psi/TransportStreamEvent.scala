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

  private def sectionsToTables[GroupingState](
    group: Transform[GroupingState, Section, Either[GroupingError, GroupedSections[Section]]], tableBuilder: TableBuilder
  ): Transform[(Map[Pid, GroupingState], TransportStreamIndex), PidStamped[Either[MpegError, Section]], TransportStreamEvent] = {

    import MpegError._

    val sectionsToTablesForPid: Transform[GroupingState, Section, Either[MpegError, TableMessage]] =
      group.map {
        case Left(e) => Left(e)
        case Right(gs) => tableBuilder.build(gs)
      }

    val sectionsToTables: Transform[Map[Pid, GroupingState], PidStamped[Either[MpegError, Section]], PidStamped[Either[MpegError, TableMessage]]] = Transform.stateful(Map.empty[Pid, GroupingState]) {
      case (state, PidStamped(pid, Right(section))) =>
        val groupingState = state.getOrElse(pid, group.initial)
        val (newGroupingState, out) = sectionsToTablesForPid.transform(groupingState, section)
        (state.updated(pid, newGroupingState), out.strict.map(o => PidStamped(pid, o)))
    }

    val withTransportStreamIndex: Transform[(Map[Pid, GroupingState], TransportStreamIndex), PidStamped[Either[MpegError, Section]], PidStamped[Either[MpegError, Either[TransportStreamIndex, TableMessage]]]] =
      sectionsToTables join PidStamped.preserve(passErrors(TransportStreamIndex.build))

    withTransportStreamIndex.map { case PidStamped(pid, value) =>
      value match {
        case Left(e) => error(pid, e)
        case Right(Left(tsi)) => metadata(tsi)
        case Right(Right(tbl)) => table(pid, tbl)
      }
    }
  }

  def fromPacketStream[GroupingState](
    sectionCodec: SectionCodec,
    group: Transform[GroupingState, Section, Either[GroupingError, GroupedSections[Section]]],
    tableBuilder: TableBuilder
  ): Transform[(Map[Pid, ContinuityCounter], Demultiplexer.State, Map[Pid, GroupingState], TransportStreamIndex), Packet, TransportStreamEvent] = {
    val demuxed: Transform[(Map[Pid, ContinuityCounter], Demultiplexer.State, Map[Pid, GroupingState], TransportStreamIndex), Packet, TransportStreamEvent] = {
      Demultiplexer.demultiplex(sectionCodec).join(
        sectionsToTables(group, tableBuilder).semipass[PidStamped[Either[DemultiplexerError, Demultiplexer.Result]], TransportStreamEvent](
          {
            case PidStamped(pid, Right(Demultiplexer.SectionResult(section))) => Right(PidStamped(pid, Right(section)))
            case PidStamped(pid, Right(Demultiplexer.PesPacketResult(p))) => Left(pes(pid, p))
            case PidStamped(pid, Left(e)) => Right(PidStamped(pid, Left(e.toMpegError)))
          })).xmapState(t => (t._1._1, t._1._2, t._2._1, t._2._2))(t => ((t._1, t._2), (t._3, t._4)))
    }

    demuxed.semipass[Packet, TransportStreamEvent]({
      case Packet(header, _, _, Some(payload)) if header.scramblingControl != 0 =>
        Left(scrambledPayload(header.pid, payload))
      case p @ Packet(_, _, _, _) =>
        Right(p)
    })
  }

  def fromSectionStream[GroupingState](
    group: Transform[GroupingState, Section, Either[GroupingError, GroupedSections[Section]]],
    tableBuilder: TableBuilder
  ): Transform[(Map[Pid, GroupingState], TransportStreamIndex), PidStamped[Section], TransportStreamEvent] =
    sectionsToTables(group, tableBuilder).contramap[PidStamped[Section]](_.map(Right(_)))
}
