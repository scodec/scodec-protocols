package scodec.protocols.mpeg
package transport

sealed trait DepacketizationError {
  def pid: Pid
  def message: String
}

object DepacketizationError {
  case class Discontinuity(pid: Pid, last: ContinuityCounter, current: ContinuityCounter) extends DepacketizationError {
    def message = s"discontinuity on pid ${pid.value} from counter $last to $current"
  }

  case class Decoding(pid: Pid, decodingError: String) extends DepacketizationError {
    def message = s"decoding error on pid ${pid.value}: $decodingError"
  }
}
