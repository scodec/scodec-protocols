package scodec.protocols

import scala.concurrent.duration._

import scalaz.{ \/, \/-, -\/ }
import \/.{ left, right }
import scalaz.concurrent.{ Strategy, Task }
import scalaz.syntax.monoid._
import scalaz.stream._
import Process._

import java.util.concurrent.ScheduledExecutorService

import org.joda.time.DateTime

/** Companion for [[TimeSeriesValue]]. */
object TimeSeriesValue {
  def apply[A](time: DateTime, value: A): TimeSeriesValue[A] = TimeStamped(time, right(value))
  def tick(time: DateTime): TimeSeriesValue[Nothing] = TimeStamped(time, left(()))
  def now[A](value: A): TimeSeriesValue[A] = TimeStamped.now(right(value))
  def nowTick: TimeSeriesValue[Nothing] = TimeStamped.now(left(()))
  def lift[A](t: TimeStamped[A]): TimeSeriesValue[A] = t map right
}

/** Companion for [[TimeSeries]]. */
object TimeSeries {

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source process. */
  def apply[A](source: Process[Task, TimeStamped[A]], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit S: Strategy, scheduler: ScheduledExecutorService): TimeSeries[A] =
    source.map { _ map right } merge timeTicks(tickPeriod).map { _ map left } pipe TimeStamped.reorderLocally(reorderOver)

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source process. */
  def lift[A](source: Process[Task, A], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit S: Strategy, scheduler: ScheduledExecutorService): TimeSeries[A] =
    apply(source map TimeStamped.now, tickPeriod, reorderOver)

  /** Stream of time ticks spaced by `tickPeriod`. */
  private def timeTicks(tickPeriod: FiniteDuration = 1.second)(implicit S: Strategy, scheduler: ScheduledExecutorService): Process[Task, TimeStamped[Unit]] =
    awakeEvery(tickPeriod) map { _ => TimeStamped.now(()) }

  /**
   * Stream transducer that converts a stream of timestamped values with monotonically increasing timestamps in
   * to a stream of timestamped ticks or values, where a tick is emitted every `tickPeriod`.
   * Ticks are emitted between values from the source stream.
   */
  def interpolateTicks[A](tickPeriod: FiniteDuration = 1.second): Process1[TimeStamped[A], TimeSeriesValue[A]] = {
    val tickPeriodMillis = tickPeriod.toMillis
    def go(nextTick: DateTime): Process1[TimeStamped[A], TimeSeriesValue[A]] = {
      await1[TimeStamped[A]] flatMap { t =>
        if (t.time.getMillis < nextTick.getMillis) emit(t map right) ++ go(nextTick)
        else {
          val tickCount = ((t.time.getMillis - nextTick.getMillis) / tickPeriodMillis + 1).toInt
          val tickTimes = (0 to tickCount) map { x => nextTick plus (x * tickPeriodMillis) }
          val ticks = tickTimes map { t => TimeStamped(t, left(())) }
          emitAll(ticks.init) ++ emit(t map right) ++ go(ticks.last.time)
        }
      }
    }
    await1[TimeStamped[A]] flatMap { t => emit(t map right) ++ go(t.time plus tickPeriodMillis) }
  }

  /**
   * Combinator that converts a `Process1[A, B]` in to a `TimeSeriesTransducer[A, B]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserve[A, B](p: Process1[A, B]): TimeSeriesTransducer[A, B] =
    TimeStamped.preserveTimeStamps(p.liftR[Unit])

  /**
   * Combinator that converts a `Process1[TimeStamped[A], TimeStamped[B]]` in to a `TimesSeriesTransducer[A, B]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTicks[A, B](p: Process1[TimeStamped[A], TimeStamped[B]]): TimeSeriesTransducer[A, B] = {
    def go(cur: Process1[TimeStamped[A], TimeStamped[B]]): TimeSeriesTransducer[A, B] = {
      receive1Or[TimeSeriesValue[A], TimeSeriesValue[B]](cur.disconnect(Cause.Kill).map(_ map right)) {
        case t @ TimeStamped(ts, -\/(())) =>
          emit(t.asInstanceOf[TimeSeriesValue[B]]) ++ go(cur)
        case t @ TimeStamped(ts, \/-(a)) =>
          val (toEmit, next) = cur.feed1(TimeStamped(ts, a)).unemit
          val out = emitAll(toEmit map { _ map right })
          next match {
            case h @ Halt(_) => out ++ h
            case other => out ++ go(other)
          }
      }
    }
    go(p)
  }
}

