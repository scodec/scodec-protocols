package scodec.protocols.mpeg
package transport

import scodec.Err

sealed abstract class DemultiplexerError extends MpegError

object DemultiplexerError {
  case class Discontinuity(last: ContinuityCounter, current: ContinuityCounter) extends DemultiplexerError {
    def message = s"pid discontinuity: $last to $current"
  }

  case class Decoding(decodingError: Err) extends DemultiplexerError {
    def message = s"decoding error: $decodingError"
  }
}
