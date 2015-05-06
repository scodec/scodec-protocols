package scodec.protocols

import scala.concurrent.duration._
import scalaz.\/
import \/.{ left, right }
import scalaz.stream._
import org.joda.time.DateTime

class TimeSeriesTest extends ProtocolsSpec {

  "the TimeSeries type" should {

    "interpolating time ticks in a timestamped stream" in {
      val events = Process.range(1, 4) map { _ * 1000L } map { x => TimeStamped(new DateTime(x), x) }
      val withTicksDefault = events.pipe(TimeSeries.interpolateTicks()).toList
      withTicksDefault shouldBe List(
        TimeStamped(new DateTime(1000), right(1000)),
        TimeStamped(new DateTime(2000), left(())),
        TimeStamped(new DateTime(2000), right(2000)),
        TimeStamped(new DateTime(3000), left(())),
        TimeStamped(new DateTime(3000), right(3000))
      )
      val withTicks300ms = events.pipe(TimeSeries.interpolateTicks(300.millis)).toList
      withTicks300ms shouldBe List(
        TimeStamped(new DateTime(1000), right(1000)),
        TimeStamped(new DateTime(1300), left(())),
        TimeStamped(new DateTime(1600), left(())),
        TimeStamped(new DateTime(1900), left(())),
        TimeStamped(new DateTime(2000), right(2000)),
        TimeStamped(new DateTime(2200), left(())),
        TimeStamped(new DateTime(2500), left(())),
        TimeStamped(new DateTime(2800), left(())),
        TimeStamped(new DateTime(3000), right(3000))
      )
    }
  }
}

