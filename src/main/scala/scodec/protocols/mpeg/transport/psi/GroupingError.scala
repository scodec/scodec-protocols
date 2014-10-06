package scodec.protocols.mpeg
package transport
package psi

case class GroupingError(tableId: Int, tableIdExtension: Int, message: String) extends MpegError
