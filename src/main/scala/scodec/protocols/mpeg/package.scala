package scodec.protocols

import scodec.{ Attempt, Codec, DecodeResult }
import scodec.bits._
import scodec.codecs._

package object mpeg {

  def reserved(bits: Int): Codec[Unit] = constantLenient(BitVector.high(bits))

  val crc32mpeg: BitVector => BitVector =
    crc(hex"04c11db7".bits, BitVector.high(32), false, false, BitVector.low(32))
}
