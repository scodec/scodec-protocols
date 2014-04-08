package scodec.protocols
package ip
package v4

import scodec.bits.ByteVector
import scodec.Codec
import scodec.codecs

case class Address(value: Int) {
  override def toString = ByteVector.fromInt(value).toIterable.map { b => 0xff & b.toInt }.mkString(".")
}

object Address {
  implicit val codec: Codec[Address] = codecs.int32.xmap[Address](v => Address(v), _.value)
}
