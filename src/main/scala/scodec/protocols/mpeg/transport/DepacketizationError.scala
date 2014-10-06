package scodec.protocols.mpeg
package transport

sealed abstract class DepacketizationError extends MpegError

object DepacketizationError {
  case class Discontinuity(last: ContinuityCounter, current: ContinuityCounter) extends DepacketizationError {
    def message = s"pid discontinuity: $last to $current"
  }

  case class Decoding(decodingError: String) extends DepacketizationError {
    def message = s"decoding error: $decodingError"
  }
}
