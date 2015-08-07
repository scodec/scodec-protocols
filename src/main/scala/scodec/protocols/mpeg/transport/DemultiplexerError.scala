package scodec.protocols.mpeg
package transport

import scodec.Err

sealed abstract class DemultiplexerError {
  def toMpegError: MpegError
}

object DemultiplexerError {

  case class Discontinuity(last: ContinuityCounter, current: ContinuityCounter) extends DemultiplexerError with MpegError {
    def message = s"pid discontinuity: $last to $current"
    def toMpegError = this
  }

  case class Decoding(decodingError: Err) extends DemultiplexerError {
    def message = s"decoding error: $decodingError"
    def toMpegError = MpegError.Decoding(decodingError)
  }
}
