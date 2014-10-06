package scodec.protocols
package mpeg

import scalaz.{ \/, \/-, -\/ }
import \/.{ left, right }
import scalaz.stream.{ Process1, process1 }

abstract class MpegError {
  def message: String
}

object MpegError {

  def joinErrors[A, B](p: Process1[A, MpegError \/ B]): Process1[MpegError \/ A, MpegError \/ B] =
    process1ext.conditionallyFeed(p, {
      case \/-(a) => left(a)
      case e @ -\/(_) => right(e)
    })

  def passErrors[A, B](p: Process1[A, B]): Process1[MpegError \/ A, MpegError \/ B] =
    process1.liftR(p)
}
