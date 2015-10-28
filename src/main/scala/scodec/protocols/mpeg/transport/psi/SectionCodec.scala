package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, -\/, \/- }
import \/.{ left, right }

import scodec.{ Attempt, Codec, Decoder, DecodeResult, DecodingContext, Err, SizeBound }
import scodec.bits._
import scodec.codecs._
import scodec.stream.decode.{ StreamDecoder, many => decodeMany }

class SectionCodec private (cases: Map[Int, SectionCodec.Case[Any, Section]], verifyCrc: Boolean = true) extends Codec[Section] {
  import SectionCodec._

  def supporting[A <: Section](implicit c: SectionFragmentCodec[A]): SectionCodec =
    new SectionCodec(cases + (c.tableId -> Case[Any, Section](
      hdr => c.subCodec(hdr).asInstanceOf[Codec[Any]],
      (privateBits, extension, data) => c.toSection(privateBits, extension, data.asInstanceOf[c.Repr]),
      section => c.fromSection(section.asInstanceOf[A])
    )), verifyCrc)

  def disableCrcVerification: SectionCodec = new SectionCodec(cases, false)

  def sizeBound = SizeBound.unknown

  def encode(section: Section) = for {
    c <- Attempt.fromOption(cases.get(section.tableId), Err(s"unsupported table id ${section.tableId}"))
    (privateBits, extension, data) = c.fromSection(section)
    preHeader = SectionHeader(section.tableId, extension.isDefined, privateBits, 0)
    encData <- extension match {
      case None => c.codec(preHeader).encode(data)
      case Some(ext) =>
        for {
          encExt <- Codec[SectionExtension].encode(ext)
          encData <- c.codec(preHeader).encode(data)
        } yield encExt ++ encData
    }
    includeCrc = extension.isDefined
    size = (encData.size / 8).toInt + (if (includeCrc) 4 else 0)
    postHeader = preHeader.copy(length = size)
    encHeader <- Codec[SectionHeader].encode(postHeader)
    withoutCrc = encHeader ++ encData
  } yield if (includeCrc) withoutCrc ++ crc32mpeg(withoutCrc) else withoutCrc

  def decode(bits: BitVector) = (for {
    header <- DecodingContext(Codec[SectionHeader])
    section <- DecodingContext.fromFunction(decodeSection(header))
  } yield section).decode(bits)

  def decodeSection(header: SectionHeader)(bits: BitVector): Attempt[DecodeResult[Section]] =
    decoder(header).decode(bits)

  def decoder(header: SectionHeader): Decoder[Section] = Decoder { bits =>

    val c = cases.getOrElse(header.tableId, unknownSectionCase(header.tableId).asInstanceOf[Case[Any, Section]])

    def ensureCrcMatches(actual: Int, expected: Int) =
      if (actual == expected) { Attempt.successful(()) }
      else Attempt.failure(Err(s"CRC mismatch: calculated $expected does not equal $actual"))

    def generateCrc(ext: SectionExtension, data: Any) = for {
      encExt <- Codec[SectionExtension].encode(ext)
      encData <- c.codec(header).encode(data)
      encHeader <- Codec[SectionHeader].encode(header)
    } yield (crc32mpeg(encHeader ++ encExt ++ encData).toInt())

    def decodeExtended: Decoder[(Option[SectionExtension], Any)] = for {
      ext <- Codec[SectionExtension]
      data <- fixedSizeBytes(header.length.toLong - 9, c.codec(header))
      actualCrc <- int32
      expectedCrc <- Decoder.liftAttempt { if (verifyCrc) generateCrc(ext, data) else Attempt.successful(actualCrc) }
      _ <- Decoder.liftAttempt(ensureCrcMatches(actualCrc, expectedCrc))
    } yield Some(ext) -> data

    def decodeStandard: Decoder[(Option[SectionExtension], Any)] = for {
      data <- fixedSizeBytes(header.length.toLong, c.codec(header))
    } yield None -> data

    for {
      result <- ( if (header.extendedSyntax) decodeExtended else decodeStandard ).decode(bits)
      DecodeResult((ext, data), remainder) = result
      section <- c.toSection(header.privateBits, ext, data)
     } yield DecodeResult(section, remainder)
  }
}

object SectionCodec {

  def empty: SectionCodec = new SectionCodec(Map.empty)

  def supporting[S <: Section : SectionFragmentCodec]: SectionCodec =
    empty.supporting[S]

  def psi: SectionCodec =
    supporting[ProgramAssociationSection].
    supporting[ProgramMapSection].
    supporting[ConditionalAccessSection]

  sealed trait UnknownSection extends Section
  case class UnknownNonExtendedSection(tableId: Int, privateBits: BitVector, data: ByteVector) extends UnknownSection
  case class UnknownExtendedSection(tableId: Int, privateBits: BitVector, extension: SectionExtension, data: ByteVector) extends UnknownSection with ExtendedSection

  private case class Case[A, B <: Section](
    codec: SectionHeader => Codec[A],
    toSection: (BitVector, Option[SectionExtension], A) => Attempt[B],
    fromSection: B => (BitVector, Option[SectionExtension], A))

  private def unknownSectionCase(tableId: Int): Case[BitVector, UnknownSection] = Case(
    hdr => bits,
    (privateBits, ext, bits) => Attempt.successful(ext match {
      case Some(e) => UnknownExtendedSection(tableId, privateBits, e, bits.bytes)
      case None =>  UnknownNonExtendedSection(tableId, privateBits, bits.bytes)
      }),
    section => section match {
      case u: UnknownExtendedSection => (u.privateBits, Some(u.extension), u.data.bits)
      case u: UnknownNonExtendedSection => (u.privateBits, None, u.data.bits)
    })
}
