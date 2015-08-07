package scodec.protocols
package mpeg

import scalaz.{ \/, \/-, -\/ }
import \/.{ left, right }
import scalaz.stream.{ Process1, process1 }
import scodec.Err

trait MpegError {
  def message: String
}

object MpegError {

  case class General(message: String) extends MpegError
  case class Decoding(err: Err) extends MpegError {
    def message = s"decoding error: $err"
  }

  def joinErrors[A, B](p: Process1[A, MpegError \/ B]): Process1[MpegError \/ A, MpegError \/ B] =
    process1ext.conditionallyFeed(p, {
      case \/-(a) => left(a)
      case e @ -\/(_) => right(e)
    })

  def passErrors[A, B](p: Process1[A, B]): Process1[MpegError \/ A, MpegError \/ B] =
    process1.liftR(p)
}
