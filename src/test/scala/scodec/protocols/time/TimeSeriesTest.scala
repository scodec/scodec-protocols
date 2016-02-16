package scodec.protocols
package time

import java.time.Instant
import scala.concurrent.duration._
import fs2._

class TimeSeriesTest extends ProtocolsSpec {

  def ts(value: Int) = TimeStamped(Instant.ofEpochSecond(value.toLong), value)

  "the TimeSeries type" should {

    "interpolating time ticks in a timestamped stream" in {
      val events = Stream(ts(1), ts(2), ts(3))
      val withTicksDefault = events.pipe(TimeSeries.interpolateTicks()).toList
      withTicksDefault shouldBe List(
        TimeStamped(Instant.ofEpochSecond(1), Some(1)),
        TimeStamped(Instant.ofEpochSecond(2), None),
        TimeStamped(Instant.ofEpochSecond(2), Some(2)),
        TimeStamped(Instant.ofEpochSecond(3), None),
        TimeStamped(Instant.ofEpochSecond(3), Some(3))
      )
      val withTicks300ms = events.pipe(TimeSeries.interpolateTicks(300.millis)).toList
      withTicks300ms shouldBe List(
        TimeStamped(Instant.ofEpochSecond(1), Some(1)),
        TimeStamped(Instant.ofEpochMilli(1300), None),
        TimeStamped(Instant.ofEpochMilli(1600), None),
        TimeStamped(Instant.ofEpochMilli(1900), None),
        TimeStamped(Instant.ofEpochSecond(2), Some(2)),
        TimeStamped(Instant.ofEpochMilli(2200), None),
        TimeStamped(Instant.ofEpochMilli(2500), None),
        TimeStamped(Instant.ofEpochMilli(2800), None),
        TimeStamped(Instant.ofEpochSecond(3), Some(3))
      )
    }
  }
}

