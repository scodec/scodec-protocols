package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/ }
import scalaz.stream._

import psi.{ Table => TableMessage }

abstract class TransportStreamEvent

object TransportStreamEvent {
  case class Table(pid: Pid, table: TableMessage) extends TransportStreamEvent
  case class Metadata[A](pid: Option[Pid], metadata: A) extends TransportStreamEvent
  case class Error(pid: Pid, err: MpegError) extends TransportStreamEvent

  def table(pid: Pid, table: TableMessage): TransportStreamEvent = Table(pid, table)
  def metadata[A](md: A): TransportStreamEvent = Metadata(None, md)
  def metadata[A](pid: Pid, md: A): TransportStreamEvent = Metadata(Some(pid), md)
  def error(pid: Pid, e: MpegError): TransportStreamEvent = Error(pid, e)

  def stream(
    sectionCodec: SectionCodec,
    tableBuilder: TableBuilder,
    group: Process1[Section, GroupingError \/ GroupedSections] = GroupedSections.group
  ): Process1[Packet, TransportStreamEvent] = {

    import MpegError._

    val sectionsToTables: Process1[PidStamped[MpegError \/ Section], PidStamped[MpegError \/ (TransportStreamIndex \/ TableMessage)]] = {

      type P = Process1[PidStamped[MpegError \/ Section], PidStamped[MpegError \/ (TransportStreamIndex \/ TableMessage)]]

      def newSectionsToTablesForPid: Process1[MpegError \/ Section, MpegError \/ (TransportStreamIndex \/ TableMessage)] =
        joinErrors(group) |> joinErrors(tableBuilder.sectionsToTables) |> passErrors(TransportStreamIndex.build)

      def go(state: Map[Pid, P]): P = {
        def cleanup = state.values.foldLeft(Process.halt: P) { (acc, p) => acc ++ p.disconnect(Cause.Kill) }
        Process.receive1Or(cleanup) {
          case event =>
            val p = state.getOrElse(event.pid, PidStamped.preservePidStamps(newSectionsToTablesForPid))
            val (toEmit, next) = p.feed1(event).unemit
            Process.emitAll(toEmit) ++ go(state + (event.pid -> next))
        }
      }

      go(Map.empty)
    }

    sectionCodec.depacketize.pipe(sectionsToTables).map {
      case PidStamped(pid, value) => value match {
        case -\/(e) => error(pid, e)
        case \/-(-\/(tsi)) => metadata(tsi)
        case \/-(\/-(tbl)) => table(pid, tbl)
      }
    }
  }
}
