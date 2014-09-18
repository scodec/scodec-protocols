package scodec.protocols.mpeg
package transport

case class DesectioningError(tableId: Int, tableIdExtension: Int, message: String)
