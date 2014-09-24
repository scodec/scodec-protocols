package scodec.protocols

import scalaz.{ Lens, Monoid }
import scalaz.std.AllInstances._
import scalaz.concurrent.Task
import scalaz.stream._
import scodec.bits._

class TimeStampedTest extends ProtocolsSpec {

  "the TimeStamped type" should {

    "support calculation of rates" which {

      "emits accumulated feature values for each specified time period and emits a final value" in {
        val data = Process.emitAll(Seq(
          TimeStamped(0, 1),
          TimeStamped(0.5, 2),
          TimeStamped(1, 1),
          TimeStamped(2.3, 2))).toSource
        data.pipe(TimeStamped.rate(1.0)(x => x)).runLog.run shouldBe Vector(
          TimeStamped(0, 3), TimeStamped(1, 1), TimeStamped(2, 2))
        data.pipe(TimeStamped.rate(2.0)(x => x)).runLog.run shouldBe Vector(TimeStamped(0, 4), TimeStamped(2, 2))
      }

      "emits 0s when values are skipped over" in {
        val data = Process.emitAll(Seq(TimeStamped(0, 1), TimeStamped(3.3, 2))).toSource
        data.pipe(TimeStamped.rate(1.0)(x => x)).runLog.run shouldBe Vector(
          TimeStamped(0, 1), TimeStamped(1, 0), TimeStamped(2, 0), TimeStamped(3, 2))
      }

      "supports calculation of an average bitrate" in {
        val data = Process.emitAll(Seq(
          TimeStamped(0, hex"deadbeef"),
          TimeStamped(1, hex"deadbeef"),
          TimeStamped(1.5, hex"deadbeef"),
          TimeStamped(2.5, hex"deadbeef"),
          TimeStamped(2.6, hex"deadbeef")
        )).toSource

        val bitsPerSecond = data.pipe(TimeStamped.rate(1.0)(x => x.size * 8))

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
