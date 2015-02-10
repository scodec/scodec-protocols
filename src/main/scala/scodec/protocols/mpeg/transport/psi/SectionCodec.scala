package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, -\/, \/- }
import \/.{ left, right }
import scalaz.stream.{ Process1, Process }

import scodec.{ Attempt, Codec, DecodeResult, DecodingContext, Err, SizeBound }
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

  private def decodeSection(header: SectionHeader)(bits: BitVector): Attempt[DecodeResult[Section]] = {

    val c = cases.getOrElse(header.tableId, unknownSectionCase(header.tableId).asInstanceOf[Case[Any, Section]])

    def ensureCrcMatches(actual: Int, expected: Int) =
      if (actual == expected) { Attempt.successful(()) }
      else Attempt.failure(Err(s"CRC mismatch: calculated $expected does not equal $actual"))

    def generateCrc(ext: SectionExtension, data: Any) = for {
      encExt <- Codec[SectionExtension].encode(ext)
      encData <- c.codec(header).encode(data)
      encHeader <- Codec[SectionHeader].encode(header)
    } yield (crc32mpeg(encHeader ++ encExt ++ encData).toInt())

    def decodeExtended: DecodingContext[(Option[SectionExtension], Any)] = for {
      ext <- DecodingContext(Codec[SectionExtension])
      data <- DecodingContext(fixedSizeBytes(header.length - 9, c.codec(header)))
      actualCrc <- DecodingContext(int32)
      expectedCrc <- DecodingContext.liftAttempt { if (verifyCrc) generateCrc(ext, data) else Attempt.successful(actualCrc) }
      _ <- DecodingContext.liftAttempt(ensureCrcMatches(actualCrc, expectedCrc))
    } yield Some(ext) -> data

    def decodeStandard: DecodingContext[(Option[SectionExtension], Any)] = for {
      data <- DecodingContext(fixedSizeBytes(header.length, c.codec(header)))
    } yield None -> data

    for {
      result <- ( if (header.extendedSyntax) decodeExtended else decodeStandard ).decode(bits)
      DecodeResult((ext, data), remainder) = result
      section <- c.toSection(header.privateBits, ext, data)
     } yield DecodeResult(section, remainder)
  }

  /**
   * Stream transducer that converts packets in to sections.
   *
   * The packets may span PID values. De-packetization is performed on each PID and as whole sections are received,
   * reassembled sections are emitted.
   *
   * Errors encountered while depacketizing are emitted.
   *
   * Upon noticing a PID discontinuity, an error is emitted and PID decoding state is discarded, resulting in any in-progress
   * section decoding to be lost for that PID.
   */
  def depacketize: Process1[Packet, PidStamped[DepacketizationError \/ Section]] = {

    type Step = Process1[PidStamped[DepacketizationError.Discontinuity] \/ Packet, PidStamped[DepacketizationError \/ Section]]

    def pidSpecificErr(pid: Pid, e: DepacketizationError): PidStamped[DepacketizationError \/ Section] =
      PidStamped(pid, left(e))

    def pidSpecificSection(pid: Pid, s: Section): PidStamped[DepacketizationError \/ Section] =
      PidStamped(pid, right(s))

    def nextSection(state: Map[Pid, SectionDecodeState], pid: Pid, payloadUnitStart: Option[Int], payload: BitVector): Step = {
      payloadUnitStart match {
        case None => go(state)
        case Some(start) =>
          val bits = payload.drop(start * 8)
          if (bits.size < 32) {
            go(state + (pid -> AwaitingSectionHeader(bits)))
          } else {
            Codec[SectionHeader].decode(bits) match {
              case Attempt.Failure(err) =>
                Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ go(state - pid)
              case Attempt.Successful(DecodeResult(header, bitsPostHeader)) =>
                sectionBody(state, pid, header, bitsPostHeader)
            }
          }
      }
    }

    def sectionBody(state: Map[Pid, SectionDecodeState], pid: Pid, header: SectionHeader, bitsPostHeader: BitVector): Step = {
      val neededBits = header.length * 8
      if (bitsPostHeader.size < neededBits) {
        go(state + (pid -> AwaitingRest(header, bitsPostHeader)))
      } else {
        decodeSection(header)(bitsPostHeader) match {
          case Attempt.Failure(err) =>
            val rest = bitsPostHeader.drop(neededBits)
            Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ potentiallyNextSection(state, pid, rest)
          case Attempt.Successful(DecodeResult(section, rest)) =>
            Process.emit(pidSpecificSection(pid, section)) ++ potentiallyNextSection(state, pid, rest)
        }
      }
    }

    def potentiallyNextSection(state: Map[Pid, SectionDecodeState], pid: Pid, payload: BitVector): Step = {
      // Peek at table_id -- if we see 0xff, then there are no further sections in this packet
      if (payload.size >= 8 && payload.take(8) != BitVector.high(8))
        nextSection(state - pid, pid, Some(0), payload)
      else go(state - pid)
    }

    def go(state: Map[Pid, SectionDecodeState]): Step =
      Process.await1[PidStamped[DepacketizationError.Discontinuity] \/ Packet].flatMap {
        case -\/(discontinuity) =>
          Process.emit(pidSpecificErr(discontinuity.pid, discontinuity.value)) ++ go(state - discontinuity.pid)

        case \/-(packet) =>
          val pid = packet.header.pid
          packet.payload match {
            case None => go(state)
            case Some(payload) =>
              state.get(packet.header.pid) match {
                case None =>
                  nextSection(state, packet.header.pid, packet.payloadUnitStart, payload)
                case Some(AwaitingSectionHeader(acc)) =>
                  nextSection(state, packet.header.pid, Some(0), acc ++ payload)
                case Some(AwaitingRest(header, acc)) =>
                  sectionBody(state, packet.header.pid, header, acc ++ payload)
              }
          }
      }

    Packet.validateContinuity pipe go(Map.empty)
  }

  /** Provides a stream decoder that decodes a bitstream of 188 byte MPEG packets in to a stream of sections. */
  def packetStreamDecoder: StreamDecoder[PidStamped[DepacketizationError \/ Section]] = decodeMany[Packet] pipe depacketize
}

object SectionCodec {

  def empty: SectionCodec = new SectionCodec(Map.empty)

  def supporting[S <: Section : SectionFragmentCodec]: SectionCodec =
    empty.supporting[S]

  def psi: SectionCodec =
    supporting[ProgramAssociationSection].
    supporting[ProgramMapSection].
    supporting[ConditionalAccessSection]

  private case class Case[A, B <: Section](
    codec: SectionHeader => Codec[A],
    toSection: (BitVector, Option[SectionExtension], A) => Attempt[B],
    fromSection: B => (BitVector, Option[SectionExtension], A))

  sealed trait UnknownSection extends Section
  case class UnknownNonExtendedSection(tableId: Int, privateBits: BitVector, data: ByteVector) extends UnknownSection
  case class UnknownExtendedSection(tableId: Int, privateBits: BitVector, extension: SectionExtension, data: ByteVector) extends UnknownSection with ExtendedSection

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

  private sealed trait SectionDecodeState
  private case class AwaitingSectionHeader(acc: BitVector) extends SectionDecodeState
  private case class AwaitingRest(header: SectionHeader, acc: BitVector) extends SectionDecodeState
}
