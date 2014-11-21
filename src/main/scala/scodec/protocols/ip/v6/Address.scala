package scodec.protocols
package ip
package v6

import scala.util.Try

import scalaz.\/
import scalaz.syntax.std.option._

import scodec.bits._
import scodec.Codec
import scodec.codecs

case class Address(bytes: ByteVector) {
  require(bytes.size == 16)

  override def toString = {
    def condense[A](xs: List[(A, Int)]): List[(A, Int, Int)] = xs match {
      case Nil => Nil
      case h :: t =>
        val segment = t takeWhile { case (x, _) => x == h._1 }
        (h._1, h._2, segment.size + 1) +: condense(t.drop(segment.size))
    }

    def show(octets: List[ByteVector]): String =
      octets.map { _.toHex.replaceAll("^0+", "0") }.mkString(":")

    val grp = bytes.grouped(2).toList

    val condensedZeroes = condense(grp.zipWithIndex).filter { case (octet, _, size) => octet == hex"0000" && size > 1 }
    if (condensedZeroes.isEmpty) {
      show(grp)
    } else {
      val (_, idx, size) = condensedZeroes.maxBy { case (_, _, size) => size }
      show(grp.take(idx)) ++ "::" ++ show(grp.drop(idx + size))
    }
  }
}

object Address {
  implicit val codec: Codec[Address] = codecs.bytes(16).as[Address]

  def fromString(str: String): String \/ Address = {
    val result = Try {
      java.net.InetAddress.getByName(str) match {
        case v6: java.net.Inet6Address => Address(ByteVector(v6.getAddress))
        case v4: java.net.Inet4Address => ip.v4.Address(ByteVector(v4.getAddress).toInt()).toV6
      }
    }.toOption
    result.toRightDisjunction(s"invalid IPv6 address: $str")
  }

  def fromStringValid(str: String): Address =
    fromString(str).valueOr { err => throw new IllegalArgumentException(err) }
}
