package scodec.protocols

import scodec.bits.BitVector

package object ip {

  /**
   * Computes the 16-bit one's complement checksum of the specified bit vector.
   * @see [[https://tools.ietf.org/html/rfc1071]]
   */
  def checksum(bits: BitVector): BitVector = {
    var sum = bits.bytes.grouped(2).foldLeft(0) { (acc, b) =>
      acc + b.toInt(signed = false)
    }
    while ((sum >> 16) != 0) {
      sum = (0xffff & sum) + (sum >> 16)
    }
    ~(BitVector.fromInt(sum).drop(16))
  }
}
