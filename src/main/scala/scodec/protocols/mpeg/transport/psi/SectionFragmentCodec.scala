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
