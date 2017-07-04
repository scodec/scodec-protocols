package scodec.protocols
package mpeg

import scodec.Err
import scodec.bits.BitVector

trait MpegError {
  def message: String
}

object MpegError {

  case class General(message: String) extends MpegError {
    override def toString = message
  }
  case class Decoding(data: BitVector, err: Err) extends MpegError {
    def message = s"error encountered when decoding: $err ${data.toHex}"
    override def toString = message
  }

  def joinErrors[S, I, O](t: Transform.Aux[S, I, Either[MpegError, O]]): Transform.Aux[S, Either[MpegError, I], Either[MpegError, O]] =
    t.semipass(_.fold(e => Left(Left(e)), i => Right(i)))

  def passErrors[S, I, O](t: Transform.Aux[S, I, O]): Transform.Aux[S, Either[MpegError, I], Either[MpegError, O]] =
    t.right
}
