package scodec.protocols
package ip
package v4

import scodec.bits.ByteVector
import scodec.Codec
import scodec.codecs

case class Address(value: Int) {
  override def toString = ByteVector.fromInt(value).toIterable.map { b => 0xff & b.toInt }.mkString(".")

  def toV6: v6.Address = v6.Address(ByteVector.low(10) ++ ByteVector.high(2) ++ ByteVector.fromInt(value))
}

object Address {
  implicit val codec: Codec[Address] = codecs.int32.xmap[Address](v => Address(v), _.value)

  def fromString(str: String): Either[String, Address] = {
    val V4Pattern = """^0*([0-9]{1,3})\.0*([0-9]{1,3})\.0*([0-9]{1,3})\.0*([0-9]{1,3})$""".r
    val result = str match {
      case V4Pattern(aa, bb, cc, dd) =>
        val (a, b, c, d) = (aa.toInt, bb.toInt, cc.toInt, dd.toInt)
        if (a >= 0 && a <= 255 && b >= 0 && b <= 255 && c >= 0 && c <= 255 && d >= 0 && d <= 255)
          Some(Address((a << 24) | (b << 16) | (c << 8) | d))
        else None
      case other =>
        None
    }
    result.fold(Left(s"invalid IPv4 address: $str"): Either[String, Address])(Right(_))
  }

  def fromStringValid(str: String): Address =
    fromString(str).fold(err => throw new IllegalArgumentException(err), identity)
}
