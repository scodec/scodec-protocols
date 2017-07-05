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

  def joinErrors[I, O](t: Transform[I, Either[MpegError, O]]): Transform.Aux[t.S, Either[MpegError, I], Either[MpegError, O]] =
    t.semipass(_.fold(e => Left(Left(e)), i => Right(i)))

  def passErrors[I, O](t: Transform[I, O]): Transform.Aux[t.S, Either[MpegError, I], Either[MpegError, O]] =
    t.right
}
