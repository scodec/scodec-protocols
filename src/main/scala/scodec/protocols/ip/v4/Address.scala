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
