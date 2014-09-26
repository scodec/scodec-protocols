package scodec.protocols.mpeg
package transport
package psi

import scalaz.\/
import scalaz.syntax.std.option._
import scodec.bits._
import scodec.Codec

trait SectionFragmentCodec[A] {
  type Repr
  def tableId: Int
  def subCodec: Codec[Repr]
  def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr): String \/ A
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
      def subCodec = Codec[Repr]
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        extension.map { ext => build(privateBits, ext, data) } \/> "extended section missing expected section extension"
      def fromSection(section: A) =
        extract(section) match { case (privateBits, ext, data) => (privateBits, Some(ext), data) }
    }
  }

  def nonExtended[A, R: Codec](tableId: Int, toSection: (BitVector, R) => A, fromSection: A => (BitVector, R)): SectionFragmentCodec[A] = {
    val tid = tableId
    val build = toSection
    val extract = fromSection
    new SectionFragmentCodec[A] {
      type Repr = R
      def tableId = tid
      def subCodec = Codec[Repr]
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        \/.right(build(privateBits, data))
      def fromSection(section: A) = {
        val (privateBits, r) = extract(section)
        (privateBits, None, r)
      }
    }
  }
}
