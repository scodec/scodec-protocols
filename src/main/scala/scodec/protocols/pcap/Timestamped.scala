package scodec.protocols.pcap

/** Value timestamped with UTC unix time. */
case class Timestamped[A](timestamp: Double, value: A)
