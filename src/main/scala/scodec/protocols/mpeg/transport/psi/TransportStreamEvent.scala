package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/ }
import scalaz.stream._

import psi.{ Table => TableMessage }

sealed abstract class TransportStreamEvent

object TransportStreamEvent {
  case class Table(pid: Pid, table: TableMessage) extends TransportStreamEvent
  case class Metadata[A](pid: Option[Pid], metadata: A) extends TransportStreamEvent
  case class Error(pid: Pid, err: MpegError) extends TransportStreamEvent

  def table(pid: Pid, table: TableMessage): TransportStreamEvent = Table(pid, table)
  def metadata[A](md: A): TransportStreamEvent = Metadata(None, md)
  def metadata[A](pid: Pid, md: A): TransportStreamEvent = Metadata(Some(pid), md)
  def error(pid: Pid, e: MpegError): TransportStreamEvent = Error(pid, e)

  def stream(sectionCodec: SectionCodec, tableBuilder: TableBuilder): Process1[Packet, TransportStreamEvent] = {
    import MpegError._
    val sectionsToTables: Process1[MpegError \/ Section, MpegError \/ (TransportStreamIndex \/ TableMessage)] =
      joinErrors(desection) |> joinErrors(tableBuilder.sectionsToTables) |> passErrors(TransportStreamIndex.build)

    sectionCodec.depacketize.pipe(PidStamped.preservePidStamps(sectionsToTables)).map {
      case PidStamped(pid, value) => value match {
        case -\/(e) => error(pid, e)
        case \/-(-\/(tsi)) => metadata(tsi)
        case \/-(\/-(tbl)) => table(pid, tbl)
      }
    }
  }
}
