package scodec.protocols

import language.implicitConversions

import scala.concurrent.duration._
import scalaz.{ Lens, Monoid, \/ }
import \/.{ left, right }
import scalaz.std.AllInstances._
import scalaz.concurrent.Task
import scalaz.stream._
import scodec.bits._
import org.joda.time.DateTime

class TimeStampedTest extends ProtocolsSpec {

  "the TimeStamped type" should {

    "support calculation of rates" which {

      implicit def intToDateTime(x: Int): DateTime = new DateTime(x * 1000)
      implicit def doubleToDateTime(x: Double): DateTime = new DateTime((x * 1000).toLong)

      "emits accumulated feature values for each specified time period and emits a final value" in {
        val data = Process.emitAll(Seq(
          TimeStamped(0, 1),
          TimeStamped(0.5, 2),
          TimeStamped(1, 1),
          TimeStamped(2.3, 2))).liftIO
        data.pipe(TimeStamped.rate(1.second)(x => x)).runLog.run shouldBe Vector(
          TimeStamped(1, 3), TimeStamped(2, 1), TimeStamped(3, 2))
        data.pipe(TimeStamped.rate(2.seconds)(x => x)).runLog.run shouldBe Vector(TimeStamped(2, 4), TimeStamped(4, 2))
      }

      "emits 0s when values are skipped over" in {
        val data = Process.emitAll(Seq(TimeStamped(0, 1), TimeStamped(3.3, 2))).liftIO
        data.pipe(TimeStamped.rate(1.second)(x => x)).runLog.run shouldBe Vector(
          TimeStamped(1, 1), TimeStamped(2, 0), TimeStamped(3, 0), TimeStamped(4, 2))

        data.pipe(TimeStamped.withRate(1.second)(x => x)).runLog.run shouldBe Vector(
          TimeStamped(0, right(1)), TimeStamped(1, left(1)), TimeStamped(2, left(0)), TimeStamped(3, left(0)), TimeStamped(3.3, right(2)), TimeStamped(4, left(2)))
      }

      "supports calculation of an average bitrate" in {
        val data = Process.emitAll(Seq(
          TimeStamped(0, hex"deadbeef"),
          TimeStamped(1, hex"deadbeef"),
          TimeStamped(1.5, hex"deadbeef"),
          TimeStamped(2.5, hex"deadbeef"),
          TimeStamped(2.6, hex"deadbeef")
        )).liftIO

        val bitsPerSecond = data.pipe(TimeStamped.rate(1.second)(x => x.size * 8))

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

    "support filtering a source of timestamped values such that output is monotonically increasing in time" which {
      def ts(value: Int) = TimeStamped(new DateTime(value), ())
      val data = Process.emitAll(Seq(0, -2, -1, 1, 5, 3, 6) map ts)

      "supports dropping out-of-order values" in {
        val filtered = data pipe TimeStamped.increasing
        filtered.toList shouldBe List(ts(0), ts(1), ts(5), ts(6))
      }

      "supports receiving out-of-order values" in {
        val filtered = data pipe TimeStamped.increasingW
        filtered.toList shouldBe List(right(ts(0)), left(ts(-2)), left(ts(-1)), right(ts(1)), right(ts(5)), left(ts(3)), right(ts(6)))
      }
    }

    "support reordering timestamped values over a specified time buffer such that output is monotonically increasing in time" which {
      def ts(value: Int) = TimeStamped(new DateTime(value), ())

      val onTheSecond = Process.range(1, 10) map { x => ts(x * 1000) }
      val onTheQuarterPast = onTheSecond map { _ mapTime { t => new DateTime(t.getMillis + 250) } }

      "reorders when all out of order values lie within the buffer time" in {
        val inOrder = onTheSecond interleave onTheQuarterPast
        val outOfOrder = onTheQuarterPast interleave onTheSecond
        val reordered = outOfOrder pipe TimeStamped.reorderLocally(1.second)
        reordered.toList shouldBe inOrder.toList
      }

      "drops values that appear outside the buffer time" in {
        // Create mostly ordered data with clumps of values around each second that are unordered
        val events = Process.range(1, 10) flatMap { x =>
          val local = (-10 to 10).map { y => ts((x * 1000) + (y * 10)) }
          Process.emitAll(util.Random.shuffle(local))
        }
        val reordered200ms = events pipe TimeStamped.reorderLocally(200.milliseconds)
        reordered200ms.toList shouldBe events.toList.sorted

        val reordered20ms = events pipe TimeStamped.reorderLocally(20.milliseconds)
        reordered20ms.toList.size should be >= 10
      }
    }
  }
}
