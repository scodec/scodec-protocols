package scodec.protocols.mpeg.transport

import scala.concurrent.duration.{FiniteDuration, MICROSECONDS}

case class Clock27MHz(value: Long) {
  def toDuration: FiniteDuration = FiniteDuration(((1000000d / 27000000) * value).toLong, MICROSECONDS)

  def +(that: Clock27MHz): Clock27MHz = Clock27MHz(value + that.value)
  def -(that: Clock27MHz): Clock27MHz = Clock27MHz(value - that.value)
}