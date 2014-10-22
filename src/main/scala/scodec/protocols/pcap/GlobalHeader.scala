package scodec.protocols.pcap

import scalaz.\/.{ left, right }
import scodec.Err
import scodec.bits.{ BitVector, ByteOrdering }
import scodec.Codec
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
    def encode(bo: ByteOrdering) =
      endiannessDependent(uint32, uint32L)(bo).encode(MagicNumber)

    def decode(buf: BitVector) =
      uint32.decode(buf).flatMap {
        case (rest, MagicNumber) => right((rest, ByteOrdering.BigEndian))
        case (rest, MagicNumberRev) => right((rest, ByteOrdering.LittleEndian))
        case (rest, other) => left(Err(s"unable to detect byte ordering due to unrecognized magic number $other"))
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
    ("network"       | Codec[LinkType] )
  }}.as[GlobalHeader]
}
