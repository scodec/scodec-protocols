package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/ }
import \/.{ left, right }
import scalaz.stream._

import psi.{ Table => TableMessage }

abstract class TransportStreamEvent

object TransportStreamEvent {
  case class Pes(pid: Pid, pes: PesPacket) extends TransportStreamEvent
  case class Table(pid: Pid, table: TableMessage) extends TransportStreamEvent
  case class Metadata[A](pid: Option[Pid], metadata: A) extends TransportStreamEvent
  case class Error(pid: Pid, err: MpegError) extends TransportStreamEvent

  def pes(pid: Pid, pes: PesPacket): TransportStreamEvent = Pes(pid, pes)
  def table(pid: Pid, table: TableMessage): TransportStreamEvent = Table(pid, table)
  def metadata[A](md: A): TransportStreamEvent = Metadata(None, md)
  def metadata[A](pid: Pid, md: A): TransportStreamEvent = Metadata(Some(pid), md)
  def error(pid: Pid, e: MpegError): TransportStreamEvent = Error(pid, e)

  private def sectionsToTables(
    group: Process1[Section, GroupingError \/ GroupedSections], tableBuilder: TableBuilder
  ): Process1[PidStamped[MpegError \/ Section], TransportStreamEvent] = {
    type SectionsToTables = Process1[PidStamped[MpegError \/ Section], PidStamped[MpegError \/ TableMessage]]
    import MpegError._

    def newSectionsToTablesForPid: SectionsToTables =
      PidStamped.preservePidStamps(joinErrors(group) |> joinErrors(tableBuilder.sectionsToTables))

    def go(state: Map[Pid, SectionsToTables]): SectionsToTables = {
      def cleanup = state.values.foldLeft(Process.halt: SectionsToTables) { (acc, p) => acc ++ p.disconnect(Cause.Kill) }
      Process.receive1Or(cleanup) {
        case event =>
          val p = state.getOrElse(event.pid, newSectionsToTablesForPid)
          val (toEmit, next) = p.feed1(event).unemit
          Process.emitAll(toEmit) ++ go(state + (event.pid -> next))
      }
    }

    val result = go(Map.empty) pipe PidStamped.preservePidStamps(passErrors(TransportStreamIndex.build))

    result map {
      case PidStamped(pid, value) => value match {
        case -\/(e) => error(pid, e)
        case \/-(-\/(tsi)) => metadata(tsi)
        case \/-(\/-(tbl)) => table(pid, tbl)
      }
    }
  }

  def fromPacketStream(
    sectionCodec: SectionCodec,
    tableBuilder: TableBuilder,
    group: Process1[Section, GroupingError \/ GroupedSections] = GroupedSections.group
  ): Process1[Packet, TransportStreamEvent] = {
    val depacketized: Process1[Packet, PidStamped[DepacketizationError \/ Depacketization.Result]] =
      Depacketization.depacketize(sectionCodec)
    depacketized pipe process1ext.conditionallyFeed[
      PidStamped[MpegError \/ Section],
      TransportStreamEvent,
      PidStamped[DepacketizationError \/ Depacketization.Result]
    ](sectionsToTables(group, tableBuilder), {
      case PidStamped(pid, \/-(Depacketization.SectionResult(section))) =>
        left(PidStamped(pid, right(section)))
      case PidStamped(pid, \/-(Depacketization.PesPacketResult(p))) =>
        right(pes(pid, p))
      case PidStamped(pid, -\/(e)) =>
        left(PidStamped(pid, left(e)))
    })
  }

  def fromSectionStream(
    tableBuilder: TableBuilder,
    group: Process1[Section, GroupingError \/ GroupedSections] = GroupedSections.group
  ): Process1[PidStamped[Section], TransportStreamEvent] = {
    process1.lift((p: PidStamped[Section]) => p map right[MpegError, Section]) pipe sectionsToTables(group, tableBuilder)
  }
}
