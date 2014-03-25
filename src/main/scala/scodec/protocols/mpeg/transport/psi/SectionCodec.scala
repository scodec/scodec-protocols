package scodec.protocols.mpeg
package transport
package psi

import scalaz.\/
import scalaz.std.AllInstances._
import scalaz.syntax.std.option._
import scodec.{ Codec, DecodingContext }
import scodec.bits._
import scodec.codecs._
import shapeless.Iso

trait SectionSubCodec[A] {
  type Repr
  def tableId: Int
  def subCodec: Codec[Repr]
  def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr): A
  def fromSection(section: A): (BitVector, Option[SectionExtension], Repr)
}

object SectionSubCodec {
  def psi[A, R: Codec](tableId: Int, toSection: (SectionExtension, R) => A, fromSection: A => (SectionExtension, R)): SectionSubCodec[A] = {
    val tid = tableId
    val build = toSection
    val extract = fromSection
    new SectionSubCodec[A] {
      type Repr = R
      def tableId = tid
      def subCodec = Codec[Repr]
      def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr) =
        build(extension.get, data)
      def fromSection(section: A) =
        extract(section) match { case (ext, data) => (bin"011", Some(ext), data) }
    }
  }
}

class SectionCodec[S <: Section] private (cases: Map[Int, SectionCodec.Case[Any, Section]]) extends Codec[S] {
  import SectionCodec._

  def supporting[A <: S](implicit c: SectionSubCodec[A]): SectionCodec[S] =
    new SectionCodec(cases + (c.tableId -> Case[Any, Section](
      c.subCodec.asInstanceOf[Codec[Any]],
      (privateBits, extension, data) => c.toSection(privateBits, extension, data.asInstanceOf[c.Repr]),
      section => c.fromSection(section.asInstanceOf[A])
    )))

  def encode(section: S) = for {
    c <- cases.get(section.tableId) \/> s"unsupported table id ${section.tableId}"
    (privateBits, extension, data) = c.fromSection(section)
    encData <- extension match {
      case None => c.codec.encode(data)
      case Some(ext) =>
        for {
          encExt <- Codec[SectionExtension].encode(ext)
          encData <- c.codec.encode(data)
          encCrc <- int32.encode(0) // TODO
        } yield encExt ++ encData ++ encCrc
    }
    header = SectionHeader(section.tableId, extension.isDefined, privateBits, (encData.size / 8).toInt)
    encHeader <- Codec[SectionHeader].encode(header)
  } yield encHeader ++ encData

  def decode(bits: BitVector) = (for {
    header <- DecodingContext(Codec[SectionHeader].decode)
    section <- DecodingContext(decodeSection(header))
  } yield section).run(bits)

  private def decodeSection(header: SectionHeader)(bits: BitVector): String \/ (BitVector, S) = {
    val c = cases.getOrElse(header.tableId, unknownSectionCase(header.tableId).asInstanceOf[Case[Any, Section]])
    val decoded: DecodingContext[(Option[SectionExtension], Any)] =
      if (header.extendedSyntax) {
        for {
          ext <- DecodingContext(Codec[SectionExtension].decode)
          data <- DecodingContext(fixedSizeBytes(header.length - 9, c.codec).decode)
          crc <- DecodingContext(int32.decode)
        } yield (Some(ext), data)
      } else {
        for {
          data <- DecodingContext(fixedSizeBytes(header.length, c.codec).decode)
        } yield (None, data)
      }

    decoded.map { case (ext, data) =>
      c.toSection(header.privateBits, ext, data).asInstanceOf[S]
    }.run(bits)
  }
}

object SectionCodec {

  def apply[S <: Section]: SectionCodec[S] = new SectionCodec(Map.empty)

  def supporting[S <: Section : SectionSubCodec]: SectionCodec[S] =
    apply[S].supporting[S]

  private case class Case[A, B <: Section](
    codec: Codec[A],
    toSection: (BitVector, Option[SectionExtension], A) => B,
    fromSection: B => (BitVector, Option[SectionExtension], A))

  private sealed trait UnknownSection extends Section
  private case class UnknownNonExtendedSection(tableId: Int, privateBits: BitVector, data: ByteVector) extends UnknownSection
  private case class UnknownExtendedSection(tableId: Int, privateBits: BitVector, extension: SectionExtension, data: ByteVector) extends UnknownSection with ExtendedSection

  private def unknownSectionCase(tableId: Int): Case[BitVector, UnknownSection] = Case(
    bits,
    (privateBits, ext, bits) => ext match {
      case Some(e) => UnknownExtendedSection(tableId, privateBits, e, bits.bytes)
      case None =>  UnknownNonExtendedSection(tableId, privateBits, bits.bytes)
      },
    section => section match {
      case u: UnknownExtendedSection => (u.privateBits, Some(u.extension), u.data.bits)
      case u: UnknownNonExtendedSection => (u.privateBits, None, u.data.bits)
    })
}
