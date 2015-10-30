package scodec.protocols.mpeg
package transport
package psi

case class GroupingError(tableId: Int, tableIdExtension: Option[Int], message: String) extends MpegError

object GroupingError {
  def apply(tableId: Int, tableIdExtension: Int, message: String): GroupingError =
    new GroupingError(tableId, Some(tableIdExtension), message)

  def apply(tableId: Int, message: String): GroupingError =
    new GroupingError(tableId, None, message)
}
