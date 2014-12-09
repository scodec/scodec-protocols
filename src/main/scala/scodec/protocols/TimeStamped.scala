package scodec.protocols

import scala.concurrent.duration._

import scalaz.{ \/, \/-, -\/, Lens, LensFamily, Monoid }
import \/.{ left, right }
import scalaz.concurrent.{ Strategy, Task }
import scalaz.syntax.monoid._
import scalaz.stream._
import Process._

import java.util.concurrent.ScheduledExecutorService

import org.joda.time.{ DateTime, DateTimeZone, Duration => JDuration }

/** Wrapper that associates a time with a value. */
case class TimeStamped[+A](time: DateTime, value: A) {
  def map[B](f: A => B): TimeStamped[B] = copy(value = f(value))
  def mapTime(f: DateTime => DateTime): TimeStamped[A] = copy(time = f(time))
}

object TimeStamped {

  def tick(time: DateTime): TimeStamped[Unit] = TimeStamped(time, ())

  def now[A](a: A): TimeStamped[A] = TimeStamped(DateTime.now(DateTimeZone.UTC), a)
  def nowTick: TimeStamped[Unit] = now(())

  def tickE(time: DateTime): TimeStamped[Unit \/ Nothing] = TimeStamped(time, left(()))
  def valueE[A](time: DateTime, value: A): TimeStamped[Unit \/ A] = TimeStamped(time, right(value))

  object Lenses {
    def TimeStamp[A]: Lens[TimeStamped[A], DateTime] = Lens.lensu((t, s) => t.copy(time = s), _.time)
    def Value[A]: Lens[TimeStamped[A], A] = Lens.lensu((t, a) => t.copy(value = a), _.value)

    def ValueMap[A, B]: LensFamily[TimeStamped[A], TimeStamped[B], A, B] =
      Lens.lensFamilyu((tsa, b) => TimeStamped(tsa.time, b), _.value)
  }

  implicit def ordering[A]: Ordering[TimeStamped[A]] = new Ordering[TimeStamped[A]] {
    def compare(x: TimeStamped[A], y: TimeStamped[A]) = x.time compareTo y.time
  }

  /**
   * Combinator that converts a `Process1[A, B]` in to a `Process1[TimeStamped[A], TimeStamped[B]]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTimeStamps[A, B](p: Process1[A, B]): Process1[TimeStamped[A], TimeStamped[B]] =
    process1ext.lensf(Lenses.ValueMap[A, B])(p)

  /**
   * Combinator that converts a `Process1[A, B]` in to a `Process1[TimeStamped[Unit \/ A], TimeStamped[Unit \/ B]]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTimeStampsAndTicks[A, B](p: Process1[A, B]): Process1[TimeStamped[Unit \/ A], TimeStamped[Unit \/ B]] =
    preserveTimeStamps(p.liftR[Unit])

  /**
   * Combinator that converts a `Process1[TimeStamped[A], TimeStamped[B]]` in to a `Process1[TimeStamped[Unit \/ A], TimeStamped[Unit \/ B]]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTimeTicks[A, B](p: Process1[TimeStamped[A], TimeStamped[B]]): Process1[TimeStamped[Unit \/ A], TimeStamped[Unit \/ B]] = {
    def go(cur: Process1[TimeStamped[A], TimeStamped[B]]): Process1[TimeStamped[Unit \/ A], TimeStamped[Unit \/ B]] = {
      receive1Or[TimeStamped[Unit \/ A], TimeStamped[Unit \/ B]](cur.disconnect(Cause.Kill).map(_ map right)) {
        case t @ TimeStamped(ts, -\/(())) => emit(t.asInstanceOf[TimeStamped[Unit \/ B]]) ++ go(cur)
        case t @ TimeStamped(ts, \/-(a)) =>
          val (toEmit, next) = cur.feed1(TimeStamped(ts, a)).unemit
          val out: Process0[TimeStamped[Unit \/ B]] = emitAll(toEmit map { _ map right })
          next match {
            case h @ Halt(_) => out ++ h
            case other => out ++ go(other)
          }
      }
    }
    go(p)
  }

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a second.
   *
   * For example, the emitted bits per second of a `Process[Task, ByteVector]` can be calculated
   * using `perSecondRate(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param f function which extracts a feature of `A`
   */
  def perSecondRate[A, B: Monoid](f: A => B): Process1[TimeStamped[A], TimeStamped[B]] =
    rate(1.second)(f)

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * For example, the emitted bits per second of a `Process[Task, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate
   * @param f function which extracts a feature of `A`
   */
  def rate[A, B: Monoid](over: FiniteDuration)(f: A => B): Process1[TimeStamped[A], TimeStamped[B]] = {
    val jodaOver = new JDuration(over.toMillis)
    def go(start: DateTime, acc: B): Process1[TimeStamped[A], TimeStamped[B]] = {
      val end = start plus jodaOver
      receive1Or[TimeStamped[A], TimeStamped[B]](emit(TimeStamped(start, acc))) {
        case t @ TimeStamped(time, a) =>
          if (time isBefore end) go(start, acc |+| f(a))
          else emit(TimeStamped(start, acc)) ++ process1.feed1(t)(go(end, Monoid[B].zero))
      }
    }
    await1[TimeStamped[A]].flatMap { first =>
      go(first.time, f(first.value))
    }
  }

  /**
   * Returns a process that is the throttled version of the source process.
   *
   * Given two adjacent items from the source process, `a` and `b`, where `a` is emitted
   * first and `b` is emitted second, their time delta is `b.time - a.time`.
   *
   * This function creates a process that emits values at wall clock times such that
   * the time delta between any two adjacent values is proportional to their time delta
   * in the source process.
   *
   * The `throttlingFactor` is a scaling factor that determines how much source time a unit
   * of wall clock time is worth. A value of 1.0 causes the output process to emit
   * values spaced in wall clock time equal to their time deltas. A value of 2.0
   * emits values at twice the speed of wall clock time.
   *
   * This is particularly useful when timestamped data can be read in bulk (e.g., from a capture file)
   * but should be "played back" at real time speeds.
   */
  def throttle[A](source: Process[Task, TimeStamped[A]], throttlingFactor: Double)(implicit S: Strategy, scheduler: ScheduledExecutorService): Process[Task, TimeStamped[A]] = {
    import wye._

    val tickDuration = 100.milliseconds
    val ticksPerSecond = 1.second.toMillis / tickDuration.toMillis

    def doThrottle: Wye[TimeStamped[A], Duration, TimeStamped[A]] = {
      def read(upto: DateTime): Wye[TimeStamped[A], Duration, TimeStamped[A]] = {
        receiveL { tsa =>
          if (tsa.time.getMillis <= upto.getMillis) emit(tsa) ++ read(upto)
          else awaitTick(upto, tsa)
        }
      }

      def awaitTick(upto: DateTime, pending: TimeStamped[A]): Wye[TimeStamped[A], Duration, TimeStamped[A]] = {
        receiveR { tick =>
          val newUpto = upto plus ((1000 / ticksPerSecond) * throttlingFactor).toLong
          if (pending.time.getMillis < newUpto.getMillis) emit(pending) ++ read(newUpto)
          else awaitTick(newUpto, pending)
        }
      }

      receiveL { tsa => emit(tsa) ++ read(tsa.time) }
    }

    (source wye awakeEvery(tickDuration))(doThrottle)
  }

  /**
   * Stream transducer that filters the specified timestamped values to ensure
   * the output time stamps are always increasing in time. Other values are
   * dropped.
   */
  def increasing[A]: Process1[TimeStamped[A], TimeStamped[A]] = increasingW.stripW

  /**
   * Stream transducer that filters the specified timestamped values to ensure
   * the output time stamps are always increasing in time. The increasing values
   * are emitted as output of the writer, while out of order values are written
   * to the writer side of the writer.
   */
  def increasingW[A]: Writer1[TimeStamped[A], TimeStamped[A], TimeStamped[A]] = {
    def notBefore(last: DateTime): Writer1[TimeStamped[A], TimeStamped[A], TimeStamped[A]] = {
      await1[TimeStamped[A]] flatMap { t =>
        val now = t.time
        if (last.getMillis <= now.getMillis) emitO(t) ++ notBefore(now)
        else emitW(t) ++ notBefore(last)
      }
    }
    await1[TimeStamped[A]] flatMap { t => emitO(t) ++ notBefore(t.time) }
  }

  /**
   * Stream transducer that reorders a stream of timestamped values that are mostly ordered,
   * using a time based buffer of the specified duration. See [[attemptReorderLocally]] for details.
   *
   * The resulting process is guaranteed to always emit values in time increasing order.
   * Values may be dropped from the source process if they were not successfully reordered.
   */
  def reorderLocally[A](over: FiniteDuration): Process1[TimeStamped[A], TimeStamped[A]] = reorderLocallyW(over).stripW

  /**
   * Stream transducer that reorders a stream of timestamped values that are mostly ordered,
   * using a time based buffer of the specified duration. See [[attemptReorderLocally]] for details.
   *
   * The resulting process is guaranteed to always emit output values in time increasing order.
   * Any values that could not be reordered due to insufficient buffer space are emitted on the writer (left)
   * side.
   */
  def reorderLocallyW[A](over: FiniteDuration): Writer1[TimeStamped[A], TimeStamped[A], TimeStamped[A]] =
    attemptReorderLocally(over) pipe increasingW

  /**
   * Stream transducer that reorders timestamped values over a specified duration.
   *
   * Values are kept in an internal buffer. Upon receiving a new value, any buffered
   * values that are timestamped with `value.time - over` are emitted. Other values,
   * and the new value, are kept in the buffer.
   *
   * This is useful for ordering mostly ordered streams, where values
   * may be out of order with close neighbors but are strictly less than values
   * that come much later in the stream.
   *
   * An example of such a structure is the result of merging streams of values generated
   * with `TimeStamped.now`.
   *
   * Caution: this transducer should only be used on streams that are mostly ordered.
   * In the worst case, if the source is in reverse order, all values in the source
   * will be accumulated in to the buffer until the source halts, and then the{
   * values will be emitted in order.
   */
  def attemptReorderLocally[A](over: FiniteDuration): Process1[TimeStamped[A], TimeStamped[A]] = {
    import scala.collection.immutable.SortedSet
    val overMillis = over.toMillis
    def go(buffered: SortedSet[TimeStamped[A]]): Process1[TimeStamped[A], TimeStamped[A]] = {
      receive1Or[TimeStamped[A], TimeStamped[A]](emitAll(buffered.toSeq)) { t =>
        val until = t.time.getMillis - overMillis
        val (toEmit, toBuffer) = buffered span { x => x.time.getMillis <= until }
        emitAll(toEmit.toSeq) ++ go(toBuffer + t)
      }
    }
    go(SortedSet.empty)
  }

  /** Stream of time ticks spaced by `tickPeriod`. */
  def timeTicks(tickPeriod: FiniteDuration = 1.second)(implicit S: Strategy, scheduler: ScheduledExecutorService): Process[Task, TimeStamped[Unit]] =
    awakeEvery(tickPeriod) map { _ => nowTick }

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source process. */
  def withTimeTicks[A](source: Process[Task, A], tickPeriod: FiniteDuration = 1.second)(implicit S: Strategy, scheduler: ScheduledExecutorService): Process[Task, TimeStamped[Unit \/ A]] =
    mergeWithTimeTicks(source map TimeStamped.now, tickPeriod)

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source process. */
  def mergeWithTimeTicks[A](source: Process[Task, TimeStamped[A]], tickPeriod: FiniteDuration = 1.second)(implicit S: Strategy, scheduler: ScheduledExecutorService): Process[Task, TimeStamped[Unit \/ A]] =
    source.map { _ map right } merge timeTicks(tickPeriod).map { _ map left }

  /**
   * Stream transducer that converts a stream of timestamped values with monotonically increasing timestamps in
   * to a stream of timestamped ticks or values, where a tick is emitted every `tickPeriod`.
   * Ticks are emitted between values from the source stream.
   */
  def interpolateTimeTicks[A](tickPeriod: FiniteDuration = 1.second): Process1[TimeStamped[A], TimeStamped[Unit \/ A]] = {
    val tickPeriodMillis = tickPeriod.toMillis
    def go(nextTick: DateTime): Process1[TimeStamped[A], TimeStamped[Unit \/ A]] = {
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
}
