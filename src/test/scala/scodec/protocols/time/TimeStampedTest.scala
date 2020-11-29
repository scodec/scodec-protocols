/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.protocols
package time

import language.implicitConversions

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import java.time.Instant
import cats.effect._
import cats.implicits._
import fs2._
import scodec.bits._

class TimeStampedTest extends ProtocolsSpec {

  group("rates") {

    implicit def intToInstant(x: Int): Instant = Instant.ofEpochSecond(x.toLong)
    implicit def doubleToInstant(x: Double): Instant = Instant.ofEpochSecond(x.toLong)

    test("emits accumulated feature values for each specified time period and emits a final value") {
      val data = Stream(
        TimeStamped(0, 1),
        TimeStamped(0.5, 2),
        TimeStamped(1, 1),
        TimeStamped(2.3, 2))

      assertEquals(data.through(TimeStamped.rate(1.second)(identity[Int]).toPipe).toVector, Vector(
        TimeStamped(1, 3), TimeStamped(2, 1), TimeStamped(3, 2)))

      assertEquals(data.through(TimeStamped.rate(2.seconds)(identity[Int]).toPipe).toVector, Vector(TimeStamped(2, 4), TimeStamped(4, 2)))
    }

    test("emits 0s when values are skipped over") {
      val data = Stream(TimeStamped(0, 1), TimeStamped(3.3, 2))
      assertEquals(data.through(TimeStamped.rate(1.second)(identity[Int]).toPipe).toVector, Vector(
        TimeStamped(1, 1), TimeStamped(2, 0), TimeStamped(3, 0), TimeStamped(4, 2)))

      assertEquals(data.through(TimeStamped.withRate(1.second)(identity[Int]).toPipe).toVector, Vector(
        TimeStamped(0, Right(1)), TimeStamped(1, Left(1)), TimeStamped(2, Left(0)), TimeStamped(3, Left(0)), TimeStamped(3.3, Right(2)), TimeStamped(4, Left(2))))
    }

    test("supports calculation of an average bitrate") {
      val data = Stream(
        TimeStamped(0, hex"deadbeef"),
        TimeStamped(1, hex"deadbeef"),
        TimeStamped(1.5, hex"deadbeef"),
        TimeStamped(2.5, hex"deadbeef"),
        TimeStamped(2.6, hex"deadbeef")
      )

      val bitsPerSecond = data.through(TimeStamped.rate(1.second)((x: ByteVector) => x.size * 8L).toPipe)

      case class Average(samples: Int, value: Double)
      val zero = Average(0, 0)
      val combineAverages = (x: Average, y: Average) => {
        val totalSamples = x.samples + y.samples
        val avg = ((x.samples * x.value) + (y.samples * y.value)) / totalSamples
        Average(totalSamples, avg)
      }

      val avgBitrate = bitsPerSecond.toVector.foldLeft(zero) { (acc, bits) => combineAverages(acc, Average(1, bits.value.toDouble)) }
      assertEqualsEpsilon(avgBitrate.value, 53.3, 1.0)
    }
  }

  group("support filtering a source of timestamped values such that output is monotonically increasing in time") {
    def ts(value: Int) = TimeStamped(Instant.ofEpochSecond(value.toLong), ())
    val data = Stream(0, -2, -1, 1, 5, 3, 6).map(ts)

    test("supports dropping out-of-order values") {
      val filtered = data through TimeStamped.increasing
      assertEquals(filtered.toList, List(ts(0), ts(1), ts(5), ts(6)))
    }

    test("supports receiving out-of-order values") {
      val filtered = data through TimeStamped.increasingW
      assertEquals(filtered.toList, List(Right(ts(0)), Left(ts(-2)), Left(ts(-1)), Right(ts(1)), Right(ts(5)), Left(ts(3)), Right(ts(6))))
    }
  }

  group("support reordering timestamped values over a specified time buffer such that output is monotonically increasing in time") {
    def ts(value: Int) = TimeStamped(Instant.ofEpochMilli(value.toLong), value.toLong)

    val onTheSecond = Stream.emits(1 to 10).map { x => ts(x * 1000) }
    val onTheQuarterPast = onTheSecond map { _ mapTime { t => t.plusMillis(250) } }

    test("reorders when all out of order values lie within the buffer time") {
      val inOrder = onTheSecond interleave onTheQuarterPast
      val outOfOrder = onTheQuarterPast interleave onTheSecond
      val reordered = outOfOrder through TimeStamped.reorderLocally(1.second)
      assertEquals(reordered.toList, inOrder.toList)
    }

    test("drops values that appear outside the buffer time") {
      // Create mostly ordered data with clumps of values around each second that are unordered
      val events = Stream.emits(1 to 10).flatMap { x =>
        val local = (-10 to 10).map { y => ts((x * 1000) + (y * 10)) }
        Stream.emits(scala.util.Random.shuffle(local))
      }
      val reordered200ms = events through TimeStamped.reorderLocally(200.milliseconds)
      assertEquals(reordered200ms.toList, events.toList.sorted)

      val reordered20ms = events through TimeStamped.reorderLocally(20.milliseconds)
      assert(reordered20ms.toList.size >= 10)
    }

    test("emits values with the same timestamp in insertion order") {
      val onTheSecondBumped = onTheSecond map { _ map { _ + 1 } }
      val inOrder = (onTheSecond interleave onTheQuarterPast) interleave (onTheSecondBumped interleave onTheQuarterPast)
      val outOfOrder = (onTheQuarterPast interleave onTheSecond) interleave (onTheQuarterPast interleave onTheSecondBumped)
      val reordered = outOfOrder through TimeStamped.reorderLocally(1.second)
      assertEquals(reordered.toList, inOrder.toList)
    }
  }

  test("support throttling a time stamped source") {
    import cats.effect.unsafe.implicits.global
    def ts(value: Int) = TimeStamped(Instant.ofEpochSecond(value.toLong), value.toLong)
    val source = Stream(ts(0), ts(1), ts(2), ts(3), ts(4)).covary[IO]
    def time[A](f: => A): Long = {
      val start = System.nanoTime
      val _ = f
      System.nanoTime - start
    }
    val realtime = source.through(TimeStamped.throttle[IO, Long](1.0)).compile.drain
    assertEqualsEpsilon(time(realtime.unsafeRunTimed(5.seconds)), 4.seconds.toNanos, 250.millis.toNanos)
    val doubletime = source.through(TimeStamped.throttle[IO, Long](2.0)).compile.drain
    assertEqualsEpsilon(time(doubletime.unsafeRunTimed(3.seconds)), 2.seconds.toNanos, 250.millis.toNanos)
  }

  test("support lifting a Transform.Aux[S, TimeStamped[A], TimeStamped[B]] in to a Transform.Aux[S, TimeStamped[Either[A, C]], TimeStamped[Either[B, C]]]") {
    val source = Stream(
      TimeStamped(Instant.ofEpochMilli(1), Left(1)),
      TimeStamped(Instant.ofEpochMilli(2), Right(2)),
      TimeStamped(Instant.ofEpochMilli(3), Right(3)),
      TimeStamped(Instant.ofEpochMilli(4), Left(4)),
      TimeStamped(Instant.ofEpochMilli(5), Left(5)),
      TimeStamped(Instant.ofEpochMilli(6), Right(6))
    )
    val square: Transform.Aux[Unit, TimeStamped[Int], TimeStamped[Int]] = Transform.lift(_.map { x => x * x })
    assertEquals(source.through(TimeStamped.left(square).toPipe).toVector, Vector(
      TimeStamped(Instant.ofEpochMilli(1), Left(1)),
      TimeStamped(Instant.ofEpochMilli(2), Right(2)),
      TimeStamped(Instant.ofEpochMilli(3), Right(3)),
      TimeStamped(Instant.ofEpochMilli(4), Left(16)),
      TimeStamped(Instant.ofEpochMilli(5), Left(25)),
      TimeStamped(Instant.ofEpochMilli(6), Right(6))
    ))
  }
}
