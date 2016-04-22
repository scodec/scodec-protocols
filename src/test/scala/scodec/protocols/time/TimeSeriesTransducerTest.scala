package scodec.protocols
package time

import fs2._
import java.time.Instant

class TimeSeriesTransducerTest extends ProtocolsSpec {

  "the TimeSeriesTransducer type" should {

    "support combining two transducers via an either" in {
      val add1: TimeSeriesTransducer[Pure, Int, Int] = TimeSeriesTransducer.lift(_ + 1)
      val add2: TimeSeriesTransducer[Pure, Int, Int] = TimeSeriesTransducer.lift(_ + 2)
      val x: TimeSeriesTransducer[Pure, Either[Int, Int], Int] = TimeSeriesTransducer.either(add1, add2)
      val source: TimeSeries[Pure, Either[Int, Int]] =
        Stream(
          TimeStamped(Instant.ofEpochMilli(0), Right(1)),
          TimeStamped(Instant.ofEpochMilli(500), Left(2)),
          TimeStamped(Instant.ofEpochMilli(1500), Right(3))
        ).through(TimeSeries.interpolateTicks())
      source.through(x).toList shouldBe List(
        TimeSeriesValue(Instant.ofEpochMilli(0), 3),
        TimeSeriesValue(Instant.ofEpochMilli(500), 3),
        TimeSeriesValue.tick(Instant.ofEpochMilli(1000)),
        TimeSeriesValue(Instant.ofEpochMilli(1500), 5))
    }
  }
}


