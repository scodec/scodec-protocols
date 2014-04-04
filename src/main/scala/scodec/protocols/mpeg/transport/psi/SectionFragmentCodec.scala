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

  def psi[A, R: Codec](tableId: Int, toSection: (SectionExtension, R) => A, fromSection: A => (SectionExtension, R)): SectionFragmentCodec[A] = {
    val tid = tableId
    val build = toSection
    val extract = fromSection
    new SectionFragmentCodec[A] {
      type Repr = R
      def tableId = tid
      def subCodec = Codec[Repr]
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        extension.map { ext => build(ext, data) } \/> "psi section missing expected section extension"
      def fromSection(section: A) =
        extract(section) match { case (ext, data) => (PsiPrivateBits, Some(ext), data) }
    }
  }
}
