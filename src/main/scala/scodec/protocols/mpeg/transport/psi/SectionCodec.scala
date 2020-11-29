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

import scodec.{ Attempt, Codec, Decoder, DecodeResult, Err, SizeBound }
import scodec.bits._
import scodec.codecs._

class SectionCodec private (cases: Map[Int, List[SectionCodec.Case[Any, Section]]], verifyCrc: Boolean = true) extends Codec[Section] {
  import SectionCodec._

  def supporting[A <: Section](implicit c: SectionFragmentCodec[A]): SectionCodec = {
    val newCases = Case.fromSectionFragmentCodec(c) :: cases.getOrElse(c.tableId, Nil)
    new SectionCodec(cases + (c.tableId -> newCases), verifyCrc)
  }

  def disableCrcVerification: SectionCodec = new SectionCodec(cases, false)

  def sizeBound = SizeBound.unknown

  def encode(section: Section) = {
    def tryEncode(c: SectionCodec.Case[Any, Section]) = {
      val (privateBits, extension, data) = c.fromSection(section)
      val preHeader = SectionHeader(section.tableId, extension.isDefined, privateBits, 0)
      for {
        encData <- extension match {
          case None => c.codec(preHeader, verifyCrc).encode(data)
          case Some(ext) =>
            for {
              encExt <- Codec[SectionExtension].encode(ext)
              encData <- c.codec(preHeader, verifyCrc).encode(data)
            } yield encExt ++ encData
        }
        includeCrc = extension.isDefined
        size = (encData.size / 8).toInt + (if (includeCrc) 4 else 0)
        postHeader = preHeader.copy(length = size)
        encHeader <- Codec[SectionHeader].encode(postHeader)
        withoutCrc = encHeader ++ encData
      } yield if (includeCrc) withoutCrc ++ crc32mpeg(withoutCrc) else withoutCrc
    }

    for {
      cs <- Attempt.fromOption(cases.get(section.tableId), Err(s"unsupported table id ${section.tableId}"))
      enc <- cs.dropRight(1).foldRight(tryEncode(cs.last)) { (next, res) => res orElse tryEncode(next) }
    } yield enc
  }

  def decode(bits: BitVector) = (for {
    header <- Codec[SectionHeader]
    section <- Decoder(decodeSection(header))
  } yield section).decode(bits)

  def decodeSection(header: SectionHeader)(bits: BitVector): Attempt[DecodeResult[Section]] =
    decoder(header).decode(bits)

  def decoder(header: SectionHeader): Decoder[Section] = {
    decoder(header, Codec.encode(header).require)
  }

  def decoder(header: SectionHeader, headerBits: BitVector): Decoder[Section] = Decoder { bits =>

    def ensureCrcMatches(actual: Int, expected: Int) =
      if (actual == expected) { Attempt.successful(()) }
      else Attempt.failure(Err(s"CRC mismatch: calculated $expected does not equal $actual"))

    def generateCrc: Int =
      crc32mpeg(headerBits ++ bits.take((header.length.toLong - 4) * 8)).toInt()

    def decodeExtended(c: SectionCodec.Case[Any, Section]): Decoder[(Option[SectionExtension], Any)] = for {
      ext <- Codec[SectionExtension]
      data <- fixedSizeBytes(header.length.toLong - 9, c.codec(header, verifyCrc))
      actualCrc <- int32
      expectedCrc = if (verifyCrc) generateCrc else actualCrc
      _ <- Decoder.liftAttempt(ensureCrcMatches(actualCrc, expectedCrc))
    } yield Some(ext) -> data

    def decodeStandard(c: SectionCodec.Case[Any, Section]): Decoder[(Option[SectionExtension], Any)] = for {
      data <- fixedSizeBytes(header.length.toLong, c.codec(header, verifyCrc))
    } yield None -> data

    def attemptDecode(c: SectionCodec.Case[Any, Section]): Attempt[DecodeResult[Section]] = for {
      result <- ( if (header.extendedSyntax) decodeExtended(c) else decodeStandard(c) ).decode(bits)
      DecodeResult((ext, data), remainder) = result
      section <- c.toSection(header.privateBits, ext, data)
    } yield DecodeResult(section, remainder)

    val cs = cases.getOrElse(header.tableId, List(unknownSectionCase(header.tableId).asInstanceOf[Case[Any, Section]]))
    cs.foldRight(attemptDecode(cs.head)) { (next, res) => res orElse attemptDecode(next) }
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
    codec: (SectionHeader, Boolean) => Codec[A],
    toSection: (BitVector, Option[SectionExtension], A) => Attempt[B],
    fromSection: B => (BitVector, Option[SectionExtension], A))

  private object Case {
    def fromSectionFragmentCodec[A <: Section](c: SectionFragmentCodec[A]): Case[Any, Section] = {
      Case[Any, Section](
        (hdr, verifyCrc) => c.subCodec(hdr, verifyCrc).asInstanceOf[Codec[Any]],
        (privateBits, extension, data) => c.toSection(privateBits, extension, data.asInstanceOf[c.Repr]),
        section => c.fromSection(section.asInstanceOf[A])
      )
    }
  }

  private def unknownSectionCase(tableId: Int): Case[BitVector, UnknownSection] = Case(
    (hdr, verifyCrc) => bits,
    (privateBits, ext, bits) => Attempt.successful(ext match {
      case Some(e) => UnknownExtendedSection(tableId, privateBits, e, bits.bytes)
      case None =>  UnknownNonExtendedSection(tableId, privateBits, bits.bytes)
      }),
    section => section match {
      case u: UnknownExtendedSection => (u.privateBits, Some(u.extension), u.data.bits)
      case u: UnknownNonExtendedSection => (u.privateBits, None, u.data.bits)
    })
}
