package scodec.protocols

import scalaz.\/
import scodec.Codec
import scodec.bits._
import scodec.codecs._

package object mpeg {

  def reserved(bits: Int): Codec[Unit] = new Codec[Unit] {
    def encode(u: Unit) = \/.right(BitVector.high(bits))
    def decode(b: BitVector) = \/.right((b drop bits, ()))
  }

  val crc32mpeg: BitVector => BitVector =
    crc(hex"04c11db7".bits, BitVector.high(32), false, false, BitVector.low(32))
}

