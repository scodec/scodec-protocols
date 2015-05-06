package scodec.protocols

import scala.concurrent.duration._

import scalaz.{ \/, \/-, -\/ }
import \/.{ left, right }
import scalaz.concurrent.{ Strategy, Task }
import scalaz.stream._
import Process._

import java.util.concurrent.ScheduledExecutorService

import org.joda.time.DateTime

/** Companion for [[TimeSeriesValue]]. */
object TimeSeriesValue {
  private val aTick: Unit \/ Nothing = left(())

  def apply[A](time: DateTime, value: A): TimeSeriesValue[A] = TimeStamped(time, right(value))
  def tick(time: DateTime): TimeSeriesValue[Nothing] = TimeStamped(time, aTick)
  def now[A](value: A): TimeSeriesValue[A] = TimeStamped.now(right(value))
  def nowTick: TimeSeriesValue[Nothing] = TimeStamped.now(aTick)
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
    time.awakeEvery(tickPeriod) map { _ => TimeStamped.now(()) }

  /**
   * Stream transducer that converts a stream of timestamped values with monotonically increasing timestamps in
   * to a stream of timestamped ticks or values, where a tick is emitted every `tickPeriod`.
   * Ticks are emitted between values from the source stream.
   */
  def interpolateTicks[A](tickPeriod: FiniteDuration = 1.second): Process1[TimeStamped[A], TimeSeriesValue[A]] = {
    val tickPeriodMillis = tickPeriod.toMillis
    def go(nextTick: DateTime): Process1[TimeStamped[A], TimeSeriesValue[A]] = {
      def tickTime(x: Int) = nextTick plus (x * tickPeriodMillis)
      await1[TimeStamped[A]] flatMap { t =>
        if (t.time.getMillis < nextTick.getMillis) emit(t map right) ++ go(nextTick)
        else {
          val tickCount = ((t.time.getMillis - nextTick.getMillis) / tickPeriodMillis + 1).toInt
          val tickTimes = (0 until tickCount) map tickTime
          val ticks = tickTimes map TimeSeriesValue.tick
          emitAll(ticks) ++ emit(t map right) ++ go(tickTime(tickCount))
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

/** Companion for [[TimeSeriesTransducer]]. */
object TimeSeriesTransducer {

  def lift[A, B](f: A => B): TimeSeriesTransducer[A, B] =
    process1.lift { _ map { _ map f } }

  def either[L, R, O](left: TimeSeriesTransducer[L, O], right: TimeSeriesTransducer[R, O]): TimeSeriesTransducer[L \/ R, O] = {
    def go(curLeft: TimeSeriesTransducer[L, O], curRight: TimeSeriesTransducer[R, O]): TimeSeriesTransducer[L \/ R, O] = {
      receive1Or[TimeSeriesValue[L \/ R], TimeSeriesValue[O]](curLeft.disconnect(Cause.Kill) ++ curRight.disconnect(Cause.Kill)) {
        case TimeStamped(time, \/-(-\/(l))) =>
          val (out, next) = curLeft.feed1(TimeSeriesValue(time, l)).unemit
          emitAll(out) ++ (next match {
            case h @ Halt(_) => h
            case _ => go(next, curRight)
          })
        case TimeStamped(time, \/-(\/-(r))) =>
          val (out, next) = curRight.feed1(TimeSeriesValue(time, r)).unemit
          emitAll(out) ++ (next match {
            case h @ Halt(_) => h
            case _ => go(curLeft, next)
          })
        case TimeStamped(time, -\/(())) =>
          val tick = TimeSeriesValue.tick(time)
          val (outL, nextL) = curLeft.feed1(tick).unemit
          val (outR, nextR) = curRight.feed1(tick).unemit
          val out = {
            var seenTicks: Set[DateTime] = Set.empty
            (outL ++ outR).sortBy { _.time.getMillis }.filter {
              case TimeStamped(time, \/-(_)) => true
              case TimeStamped(time, -\/(_)) if seenTicks contains time => false
              case TimeStamped(time, -\/(_)) =>
                seenTicks += time
                true
            }
          }
          emitAll(out) ++ {
            (nextL, nextR) match {
              case (h @ Halt(_), _) => h
              case (_, h @ Halt(_)) => h
              case _ => go(nextL, nextR)
            }
          }
      }
    }
    go(left, right)
  }
}

