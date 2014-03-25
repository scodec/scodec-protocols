package scodec.protocols

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._

package object mpeg {

  def reserved(bits: Int): Codec[Unit] =
    constant(BitVector.high(bits))
}

