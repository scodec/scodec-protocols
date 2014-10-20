package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/ }
import scalaz.\/.{ right, left }
import scalaz.std.AllInstances._
import scalaz.syntax.std.option._
import scalaz.syntax.either._

import scalaz.stream.{ Process1, Process }

import scodec.{ Codec, DecodingContext }
import scodec.bits._
import scodec.codecs._
import scodec.stream.decode.{ StreamDecoder, many => decodeMany }

class SectionCodec private (cases: Map[Int, SectionCodec.Case[Any, Section]]) extends Codec[Section] {
  import SectionCodec._

  def supporting[A <: Section](implicit c: SectionFragmentCodec[A]): SectionCodec =
    new SectionCodec(cases + (c.tableId -> Case[Any, Section](
      c.subCodec.asInstanceOf[Codec[Any]],
      (privateBits, extension, data) => c.toSection(privateBits, extension, data.asInstanceOf[c.Repr]),
      section => c.fromSection(section.asInstanceOf[A])
    )))

  def encode(section: Section) = for {
    c <- cases.get(section.tableId) \/> s"unsupported table id ${section.tableId}"
    (privateBits, extension, data) = c.fromSection(section)
    encData <- extension match {
      case None => c.codec.encode(data)
      case Some(ext) =>
        for {
          encExt <- Codec[SectionExtension].encode(ext)
          encData <- c.codec.encode(data)
        } yield encExt ++ encData
    }
    includeCrc = extension.isDefined
    size = (encData.size / 8).toInt + (if (includeCrc) 4 else 0)
    header = SectionHeader(section.tableId, extension.isDefined, privateBits, size)
    encHeader <- Codec[SectionHeader].encode(header)
    withoutCrc = encHeader ++ encData
  } yield if (includeCrc) withoutCrc ++ crc32mpeg(withoutCrc) else withoutCrc

  def decode(bits: BitVector) = (for {
    header <- DecodingContext(Codec[SectionHeader].decode)
    section <- DecodingContext(decodeSection(header))
  } yield section).run(bits)

  private def decodeSection(header: SectionHeader)(bits: BitVector): String \/ (BitVector, Section) = {

    val c = cases.getOrElse(header.tableId, unknownSectionCase(header.tableId).asInstanceOf[Case[Any, Section]])

    def ensureCrcMatches(crc: Int, genCrc: Int) = if (crc == genCrc) { println("CRC matches");().right }else s"CRC ($crc) does not match generated CRC ($genCrc)".left

    def generateCrc(ext: SectionExtension, data: Any) = for {
      encExt <- Codec[SectionExtension].encode(ext)
      encData <- c.codec.encode(data)
      encHeader <- Codec[SectionHeader].encode(header)
    } yield crc32mpeg(encHeader ++ encExt ++ encData)

    def decodeExtended: DecodingContext[(Option[SectionExtension], Any)] = for {
      ext <- DecodingContext(Codec[SectionExtension].decode)
      data <- DecodingContext(fixedSizeBytes(header.length - 9, c.codec).decode)
      crc <- DecodingContext(int32.decode)
      genCrc <- DecodingContext.liftE(generateCrc(ext, data))
      _ <- DecodingContext.liftE(ensureCrcMatches(crc, genCrc.toInt()))
    } yield Some(ext) -> data

    def decodeStandard: DecodingContext[(Option[SectionExtension], Any)] = for {
      data <- DecodingContext(fixedSizeBytes(header.length, c.codec).decode)
    } yield None -> data

    for {
      result <- ( if (header.extendedSyntax) decodeExtended else decodeStandard ).run(bits)
      (remainingBits, (ext, data)) = result
       section <- c.toSection(header.privateBits, ext, data)
     } yield remainingBits -> section
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
              case -\/(err) =>
                Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ go(state - pid)
              case \/-((bitsPostHeader, header)) =>
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
          case -\/(err) =>
            val rest = bitsPostHeader.drop(neededBits)
            Process.emit(pidSpecificErr(pid, DepacketizationError.Decoding(err))) ++ potentiallyNextSection(state, pid, rest)
          case \/-((rest, section)) =>
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

  def psi: SectionCodec = supporting[ProgramAssociationSection].supporting[ProgramMapSection].supporting[ConditionalAccessSection]

  private case class Case[A, B <: Section](
    codec: Codec[A],
    toSection: (BitVector, Option[SectionExtension], A) => String \/ B,
    fromSection: B => (BitVector, Option[SectionExtension], A))

  sealed trait UnknownSection extends Section
  case class UnknownNonExtendedSection(tableId: Int, privateBits: BitVector, data: ByteVector) extends UnknownSection
  case class UnknownExtendedSection(tableId: Int, privateBits: BitVector, extension: SectionExtension, data: ByteVector) extends UnknownSection with ExtendedSection

  private def unknownSectionCase(tableId: Int): Case[BitVector, UnknownSection] = Case(
    bits,
    (privateBits, ext, bits) => right(ext match {
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
