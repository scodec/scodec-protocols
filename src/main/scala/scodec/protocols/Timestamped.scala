package scodec.protocols

/** Value timestamped with UTC time. */
case class Timestamped[A](timestamp: Double, value: A)
