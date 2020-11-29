/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.protocols
package ip
package v6

import scala.util.Try

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

  def fromString(str: String): Either[String, Address] = {
    // FIXME: this implementation erroneously supports hostnames and can be slow as a result
    val result = Try {
      java.net.InetAddress.getByName(str) match {
        case v6: java.net.Inet6Address => Address(ByteVector(v6.getAddress))
        case v4: java.net.Inet4Address => ip.v4.Address(ByteVector(v4.getAddress).toInt()).toV6
      }
    }.toOption
    result.map(Right.apply).getOrElse(Left(s"invalid IPv6 address: $str"))
  }

  def fromStringValid(str: String): Address =
    fromString(str).fold(err => throw new IllegalArgumentException(err), identity)
}
