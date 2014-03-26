package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/ }
import scalaz.\/.{ right, left }
import scalaz.std.AllInstances._
import scalaz.syntax.std.option._

import scalaz.stream.{ Process1, Process }

import scodec.{ Codec, DecodingContext }
import scodec.bits._
import scodec.codecs._

import shapeless.Iso

trait SectionSubCodec[A] {
  type Repr
  def tableId: Int
  def subCodec: Codec[Repr]
  def toSection(privateBits: BitVector, extension: Option[SectionExtension], data: Repr): String \/ A
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
        extension.map { ext => build(ext, data) } \/> "psi section missing expected section extension"
      def fromSection(section: A) =
        extract(section) match { case (ext, data) => (bin"011", Some(ext), data) }
    }
  }
}

class SectionCodec private (cases: Map[Int, SectionCodec.Case[Any, Section]]) extends Codec[Section] {
  import SectionCodec._

  def supporting[A <: Section](implicit c: SectionSubCodec[A]): SectionCodec =
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

  private def decodeSection(header: SectionHeader)(bits: BitVector): String \/ (BitVector, Section) = {
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

    decoded.flatMap { case (ext, data) =>
      DecodingContext { bits =>
        c.toSection(header.privateBits, ext, data).map { res => (bits, res) }
      }
    }.run(bits)
  }

  def depacketize: Process1[Packet, Section] = {

    def nextSection(state: Map[Pid, SectionDecodeState], pid: Pid, payloadUnitStart: Option[Int], payload: BitVector): Process1[Packet, Section] = {
      payloadUnitStart match {
        case None => go(state)
        case Some(start) =>
          val bits = payload.drop(start * 8)
          if (bits.size < 32) {
            go(state + (pid -> AwaitingSectionHeader(bits)))
          } else {
            Codec[SectionHeader].decode(bits) match {
              case -\/(err) =>
                // TODO report err
                go(state - pid)
              case \/-((bitsPostHeader, header)) =>
                sectionBody(state, pid, header, bitsPostHeader)
            }
          }
      }
    }

    def sectionBody(state: Map[Pid, SectionDecodeState], pid: Pid, header: SectionHeader, bitsPostHeader: BitVector) : Process1[Packet, Section] = {
      val neededBits = header.length * 8
      if (bitsPostHeader.size < neededBits) {
        go(state + (pid -> AwaitingRest(header, bitsPostHeader)))
      } else {
        decodeSection(header)(bitsPostHeader) match {
          case -\/(err) =>
            // TODO report err
            go(state - pid)
          case \/-((rest, section)) =>
            Process.emit(section) ++ {
              // Peek at table_id -- if we see 0xff, then there are no further sections in this packet
              if (rest.size >= 8 && rest.take(8) != BitVector.high(8))
                nextSection(state - pid, pid, Some(0), rest)
              else go(state - pid)
            }
        }
      }
    }

    def go(state: Map[Pid, SectionDecodeState]): Process1[Packet, Section] = Process.await1[Packet].flatMap { packet =>
      packet.payload match {
        case None => go(state)
        case Some(payload) =>
          state.get(packet.header.pid) match {
            case None => nextSection(state, packet.header.pid, packet.payloadUnitStart, payload)
            case Some(AwaitingSectionHeader(acc)) =>
              nextSection(state, packet.header.pid, Some(0), acc ++ payload)
            case Some(AwaitingRest(header, acc)) =>
              sectionBody(state, packet.header.pid, header, acc ++ payload)
          }
      }
    }

    go(Map.empty)
  }
}

object SectionCodec {

  def empty: SectionCodec = new SectionCodec(Map.empty)

  def supporting[S <: Section : SectionSubCodec]: SectionCodec =
    empty.supporting[S]

  private case class Case[A, B <: Section](
    codec: Codec[A],
    toSection: (BitVector, Option[SectionExtension], A) => String \/ B,
    fromSection: B => (BitVector, Option[SectionExtension], A))

  private sealed trait UnknownSection extends Section
  private case class UnknownNonExtendedSection(tableId: Int, privateBits: BitVector, data: ByteVector) extends UnknownSection
  private case class UnknownExtendedSection(tableId: Int, privateBits: BitVector, extension: SectionExtension, data: ByteVector) extends UnknownSection with ExtendedSection

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
