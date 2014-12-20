package scodec.protocols

import scalaz.\/
import \/.{ left, right }
import scalaz.stream._
import Process._
import org.joda.time.DateTime

class TimeSeriesTransducerTest extends ProtocolsSpec {

  "the TimeSeriesTransducer type" should {

    "support combining two transducers via an either" in {
      val add1: TimeSeriesTransducer[Int, Int] = TimeSeriesTransducer.lift(_ + 1)
      val add2: TimeSeriesTransducer[Int, Int] = TimeSeriesTransducer.lift(_ + 2)
      val x: TimeSeriesTransducer[Int \/ Int, Int] = TimeSeriesTransducer.either(add1, add2)
      val source: TimeSeries[Int \/ Int] =
        emitAll(List(
          TimeStamped(new DateTime(0), right(1)),
          TimeStamped(new DateTime(500), left(2)),
          TimeStamped(new DateTime(1500), right(3))
        )).pipe(TimeSeries.interpolateTicks()).toSource
      source.pipe(x).runLog.run shouldBe Vector(
        TimeSeriesValue(new DateTime(0), 3),
        TimeSeriesValue(new DateTime(500), 3),
        TimeSeriesValue.tick(new DateTime(1000)),
        TimeSeriesValue(new DateTime(1500), 5))
    }
  }
}


