package scodec.protocols

import scodec.bits.ByteOrdering
import scodec.Codec
import scodec.codecs._

/**
 * Protocol that describes libpcap files.
 *
 * @see http://wiki.wireshark.org/Development/LibpcapFileFormat
 */
package object pcap {

  def gint16(implicit ordering: ByteOrdering): Codec[Int] = endiannessDependent(int16, int16L)
  def guint16(implicit ordering: ByteOrdering): Codec[Int] = endiannessDependent(uint16, uint16L)
  def gint32(implicit ordering: ByteOrdering): Codec[Int] = endiannessDependent(int32, int32L)
  def guint32(implicit ordering: ByteOrdering): Codec[Long] = endiannessDependent(uint32, uint32L)
}
