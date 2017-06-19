package scodec.protocols.pcap

import scodec.Err
import scodec.bits.{ BitVector, ByteOrdering }
import scodec.{ Attempt, Codec, DecodeResult, SizeBound }
import scodec.codecs._

case class GlobalHeader(
  ordering: ByteOrdering,
  versionMajor: Int,
  versionMinor: Int,
  thiszone: Int,
  sigfigs: Long,
  snaplen: Long,
  network: LinkType)

object GlobalHeader {
  private val MagicNumber = 0xa1b2c3d4L
  private val MagicNumberRev = 0xd4c3b2a1L

  private val byteOrdering: Codec[ByteOrdering] = new Codec[ByteOrdering] {
    def sizeBound = SizeBound.exact(32)

    def encode(bo: ByteOrdering) =
      endiannessDependent(uint32, uint32L)(bo).encode(MagicNumber)

    def decode(buf: BitVector) =
      uint32.decode(buf).flatMap {
        case DecodeResult(MagicNumber, rest) => Attempt.successful(DecodeResult(ByteOrdering.BigEndian, rest))
        case DecodeResult(MagicNumberRev, rest) => Attempt.successful(DecodeResult(ByteOrdering.LittleEndian, rest))
        case DecodeResult(other, rest) => Attempt.failure(Err(s"unable to detect byte ordering due to unrecognized magic number $other"))
      }

    override def toString = "byteOrdering"
  }

  implicit val codec: Codec[GlobalHeader] = "global-header" | {
    ("magic_number"  | byteOrdering    ) >>:~ { implicit ordering =>
    ("version_major" | guint16         ) ::
    ("version_minor" | guint16         ) ::
    ("thiszone"      | gint32          ) ::
    ("sigfigs"       | guint32         ) ::
    ("snaplen"       | guint32         ) ::
    ("network"       | LinkType.codec  )
  }}.as[GlobalHeader]
}
