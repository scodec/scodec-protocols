package scodec.protocols.mpeg
package transport
package psi

case class DesectioningError(tableId: Int, tableIdExtension: Int, message: String) extends MpegError
