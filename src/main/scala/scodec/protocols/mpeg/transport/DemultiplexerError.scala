package scodec.protocols.mpeg
package transport

import scodec.Err
import scodec.bits.BitVector

sealed abstract class DemultiplexerError {
  def toMpegError: MpegError
}

object DemultiplexerError {

  case class Discontinuity(last: ContinuityCounter, current: ContinuityCounter, adaptationFieldControl: Int) extends DemultiplexerError with MpegError {
    def message = s"pid discontinuity: $last to $current with adaptation field control $adaptationFieldControl"
    def toMpegError = this
  }

  case class Decoding(data: BitVector, decodingError: Err) extends DemultiplexerError {
    def message = s"decoding error ($decodingError) while decoding ${data.toHex}"
    def toMpegError = MpegError.Decoding(data, decodingError)
  }
}
