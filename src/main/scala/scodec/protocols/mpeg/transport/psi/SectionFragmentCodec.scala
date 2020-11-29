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

package scodec.protocols.mpeg
package transport
package psi

import scodec.bits._
import scodec.{ Attempt, Codec, Err }

trait SectionFragmentCodec[A] {
  type Repr
  def tableId: Int
  def subCodec(header: SectionHeader, verifyCrc: Boolean): Codec[Repr]
  def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr): Attempt[A]
  def fromSection(section: A): (BitVector, Option[SectionExtension], Repr)
}

object SectionFragmentCodec {
  private val PsiPrivateBits = bin"011"

  def psi[A, R: Codec](tableId: Int, toSection: (SectionExtension, R) => A, fromSection: A => (SectionExtension, R)): SectionFragmentCodec[A] =
    extended[A, R](tableId, (_, ext, r) => toSection(ext, r), s => { val (ext, r) = fromSection(s); (PsiPrivateBits, ext, r) })

  def extended[A, R: Codec](tableId: Int, toSection: (BitVector, SectionExtension, R) => A, fromSection: A => (BitVector, SectionExtension, R)): SectionFragmentCodec[A] = {
    val tid = tableId
    val build = toSection
    val extract = fromSection
    new SectionFragmentCodec[A] {
      type Repr = R
      def tableId = tid
      def subCodec(header: SectionHeader, verifyCrc: Boolean) = Codec[Repr]
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        Attempt.fromOption(extension.map { ext => build(privateBits, ext, data) }, Err("extended section missing expected section extension"))
      def fromSection(section: A) =
        extract(section) match { case (privateBits, ext, data) => (privateBits, Some(ext), data) }
    }
  }

  def nonExtended[A, R](tableId: Int, toCodec: SectionHeader => Codec[R], toSection: (BitVector, R) => A, fromSection: A => (BitVector, R)): SectionFragmentCodec[A] = {
    val tid = tableId
    val codec = toCodec
    val build = toSection
    val extract = fromSection
    new SectionFragmentCodec[A] {
      type Repr = R
      def tableId = tid
      def subCodec(header: SectionHeader, verifyCrc: Boolean) = codec(header)
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        Attempt.successful(build(privateBits, data))
      def fromSection(section: A) = {
        val (privateBits, r) = extract(section)
        (privateBits, None, r)
      }
    }
  }

  def nonExtendedWithCrc[A, R](tableId: Int, toCodec: (SectionHeader, Boolean) => Codec[R], toSection: (BitVector, R) => A, fromSection: A => (BitVector, R)): SectionFragmentCodec[A] = {
    val tid = tableId
    val codec = toCodec
    val build = toSection
    val extract = fromSection
    new SectionFragmentCodec[A] {
      type Repr = R
      def tableId = tid
      def subCodec(header: SectionHeader, verifyCrc: Boolean) = codec(header, verifyCrc)
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        Attempt.successful(build(privateBits, data))
      def fromSection(section: A) = {
        val (privateBits, r) = extract(section)
        (privateBits, None, r)
      }
    }
  }

  def nonExtendedIdentity[A](tableId: Int, toCodec: SectionHeader => Codec[A]): SectionFragmentCodec[A] =
    SectionFragmentCodec.nonExtended[A, A](tableId, sHdr => toCodec(sHdr), (bits, a) => a, a => (BitVector.empty, a))

  def nonExtendedIdentityWithCrc[A](tableId: Int, toCodec: (SectionHeader, Boolean) => Codec[A]): SectionFragmentCodec[A] =
    SectionFragmentCodec.nonExtendedWithCrc[A, A](tableId, (sHdr, verifyCrc) => toCodec(sHdr, verifyCrc), (bits, a) => a, a => (BitVector.empty, a))
}
