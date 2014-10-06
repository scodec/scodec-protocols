package scodec.protocols
package mpeg

sealed abstract class MpegEvent

object MpegEvent {
  case class Table(table: Table) extends MpegEvent
  case class Metadata[A](metadata: A) extends MpegEvent
  case class Error(err: MpegError) extends MpegEvent

  def table(table: Table): MpegEvent = Table(table)
  def metadata[A](md: A): MpegEvent = Metadata(md)
  def error(e: MpegError): MpegEvent = Error(e)
}
