package scodec.protocols

import scalaz.{ Lens, Monoid }
import scalaz.std.AllInstances._
import scalaz.concurrent.Task
import scalaz.stream._
import scodec.bits._

class TimestampedTest extends ProtocolsSpec {

  "the Timestamped type" should {

    "support calculation of rates" which {

      "emits accumulated feature values for each specified time period and emits a final value" in {
        val data = Process.emitAll(Seq(Timestamped(0, 1), Timestamped(0.5, 2), Timestamped(1, 1), Timestamped(2.3, 2))).toSource
        data.pipe(Timestamped.rate(1.0)(x => x)).runLog.run shouldBe Vector(Timestamped(0, 3), Timestamped(1, 1), Timestamped(2, 2))
        data.pipe(Timestamped.rate(2.0)(x => x)).runLog.run shouldBe Vector(Timestamped(0, 4), Timestamped(2, 2))
      }

      "emits 0s when values are skipped over" in {
        val data = Process.emitAll(Seq(Timestamped(0, 1), Timestamped(3.3, 2))).toSource
        data.pipe(Timestamped.rate(1.0)(x => x)).runLog.run shouldBe Vector(Timestamped(0, 1), Timestamped(1, 0), Timestamped(2, 0), Timestamped(3, 2))
      }

      "supports calculation of an average bitrate" in {
        val data = Process.emitAll(Seq(
          Timestamped(0, hex"deadbeef"),
          Timestamped(1, hex"deadbeef"),
          Timestamped(1.5, hex"deadbeef"),
          Timestamped(2.5, hex"deadbeef"),
          Timestamped(2.6, hex"deadbeef")
        )).toSource

        val bitsPerSecond = data.pipe(Timestamped.rate(1.0)(x => x.size * 8))

        case class Average(samples: Int, value: Double)
        implicit val avgMonoid: Monoid[Average] = Monoid.instance((x, y) => {
          val yy = y
          val totalSamples = x.samples + yy.samples
          val avg = ((x.samples * x.value) + (yy.samples * yy.value)) / totalSamples
          Average(totalSamples, avg)
        }, Average(0, 0))

        val avgBitrate = bitsPerSecond.runFoldMap { bits => Average(1, bits.value) }.run
        avgBitrate.value shouldBe 53.3 +- 1.0
      }
    }
  }
}
