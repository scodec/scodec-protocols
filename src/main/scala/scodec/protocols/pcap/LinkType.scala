package scodec.protocols.pcap

import scodec.Codec
import scodec.bits.ByteOrdering

/**
 * Describes the link layer type in a PCAP capture.
 * @see http://www.tcpdump.org/linktypes.html
 */
sealed trait LinkType

/** Companion for [[LinkType]]. */
object LinkType {
  case object Null extends LinkType
  case object Ethernet extends LinkType
  case object Raw extends LinkType
  case object IPv4 extends LinkType
  case object IPv6 extends LinkType
  case object MPEG2TS extends LinkType
  case class Unknown(value: Long) extends LinkType

  def toLong(lt: LinkType): Long = lt match {
    case Null => 0
    case Ethernet => 1
    case Raw => 101
    case IPv4 => 228
    case IPv6 => 229
    case MPEG2TS => 243
    case Unknown(value) => value
  }

  def fromLong(l: Long): LinkType = l match {
    case 0 => Null
    case 1 => Ethernet
    case 101 => Raw
    case 228 => IPv4
    case 229 => IPv6
    case 243 => MPEG2TS
    case other => Unknown(other)
  }

  implicit def codec(implicit bo: ByteOrdering): Codec[LinkType] =
    guint32.xmap[LinkType](fromLong, toLong)
}
