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

import language.higherKinds

import java.time.Instant

import scala.concurrent.duration._

import cats.Functor
import cats.effect.Temporal

import fs2._

/** Companion for [[TimeSeries]]. */
object TimeSeries {

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source stream. */
  def apply[F[_]: Temporal, A](source: Stream[F, TimeStamped[A]], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds): TimeSeries[F, A] = {
    val src: TimeSeries[F, A] = source.map(tsa => tsa.map(Some(_): Option[A]))
    val ticks: TimeSeries[F, Nothing] = timeTicks(tickPeriod).map(tsu => tsu.map(_ => None))
    src merge ticks through TimeStamped.reorderLocally(reorderOver)
  }

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source stream. */
  def lift[F[_]: Temporal, A](source: Stream[F, A], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds): TimeSeries[F, A] =
    apply(source map TimeStamped.now, tickPeriod, reorderOver)

  /** Stream of time ticks spaced by `tickPeriod`. */
  private def timeTicks[F[_]: Temporal](tickPeriod: FiniteDuration): Stream[F, TimeStamped[Unit]] =
    Stream.awakeEvery[F](tickPeriod) map { _ => TimeStamped.now(()) }

  /**
   * Stream transducer that converts a stream of timestamped values with monotonically increasing timestamps in
   * to a stream of timestamped ticks or values, where a tick is emitted every `tickPeriod`.
   * Ticks are emitted between values from the source stream.
   */
  def interpolateTicks[A](tickPeriod: FiniteDuration = 1.second): Pipe[Pure, TimeStamped[A], TimeSeriesValue[A]] = {
    val tickPeriodMillis = tickPeriod.toMillis
    def go(nextTick: Instant, s: Stream[Pure, TimeStamped[A]]): Pull[Pure, TimeSeriesValue[A], Unit] = {
      def tickTime(x: Int) = nextTick plusMillis (x * tickPeriodMillis)
      s.pull.uncons.flatMap {
        case Some((hd,tl)) =>
          hd.indexWhere(_.time.toEpochMilli >= nextTick.toEpochMilli) match {
            case None =>
              if (hd.isEmpty) Pull.pure(()) else Pull.output(hd.map(_.toTimeSeriesValue)) >> go(nextTick, tl)
            case Some(idx) =>
              val (prefix, suffix) = hd.splitAt(idx)
              val out = if (prefix.isEmpty) Pull.pure(()) else Pull.output(prefix.map(_.toTimeSeriesValue))
              // we know suffix is non-empty and suffix.head has a time >= next tick time
              val next = suffix(0)
              val tickCount = ((next.time.toEpochMilli - nextTick.toEpochMilli) / tickPeriodMillis + 1).toInt
              val tickTimes = (0 until tickCount) map tickTime
              val ticks = tickTimes map TimeSeriesValue.tick
              val rest = Pull.output(Chunk.seq(ticks)) >> go(tickTime(tickCount), tl.cons(suffix))
              out >> rest
          }
        case None => Pull.done
      }
    }
    in => in.pull.uncons1.flatMap {
      case Some((hd,tl)) => Pull.output1(hd.toTimeSeriesValue) >> go(hd.time plusMillis tickPeriodMillis, tl)
      case None => Pull.done
    }.stream
  }

  /**
   * Combinator that converts a `Transform[I, O]` in to a `Transform[TimeSeriesValue[I], TimeSeriesValue[O]]` such that
   * timestamps are preserved on elements that flow through the stream.
   */
  def preserve[I, O](t: Transform[I, O]): Transform.Aux[t.S, TimeSeriesValue[I], TimeSeriesValue[O]] =
    preserveTicks(TimeStamped.preserve(t))

  /**
   * Combinator that converts a `Transform[TimeStamped[A], TimeStamped[B]]` in to a `Transform[TimeSeriesValue[A], TimeSeriesValue[B]]` such that
   * timestamps are preserved on elements that flow through the stream.
   */
  def preserveTicks[I, O](t: Transform[TimeStamped[I], TimeStamped[O]]): Transform.Aux[t.S, TimeSeriesValue[I], TimeSeriesValue[O]] =
    t.semilens(
      tsi => tsi.value.map(v => Right(TimeStamped(tsi.time, v))).getOrElse(Left(TimeSeriesValue.tick(tsi.time))),
      (_, tso) => tso.map(Some(_)))

  /**
   * Combinator that combines a `Transform[TimeSeriesValue[L],O]` and a `Transform[TimeSeriesValue[R],O]` in to a `Transform[TimeSeriesVlaue[Either[L,R],O]]`.
   */
  def choice[L,R,O](l: Transform[TimeSeriesValue[L],O], r: Transform[TimeSeriesValue[R],O]): Transform[TimeSeriesValue[Either[L, R]], O] =
    Transform[(l.S, r.S), TimeSeriesValue[Either[L, R]], O]((l.initial, r.initial))({ case ((lState, rState), tsv) =>
      tsv match {
        case TimeStamped(t, Some(Left(lValue))) => 
          val (s, out) = l.transform(lState, TimeStamped(t, Some(lValue)))
          (s -> rState, out)
        case TimeStamped(t, Some(Right(rValue))) =>
          val (s, out) = r.transform(rState, TimeStamped(t, Some(rValue)))
          (lState -> s, out)
        case TimeStamped(t, None) =>
          val (ls, lout) = l.transform(lState, TimeStamped(t, None))
          val (rs, rout) = r.transform(rState, TimeStamped(t, None))
          ((ls, rs), lout ++ rout)
      }
    }, { case (lState, rState) => l.onComplete(lState) ++ r.onComplete(rState) })
}
