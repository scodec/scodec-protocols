package scodec.protocols
package time

import java.time.Instant

import scala.concurrent.duration._

import fs2._
import fs2.process1.Stepper
import fs2.util.Task

import java.util.concurrent.ScheduledExecutorService

/** Companion for [[TimeSeries]]. */
object TimeSeries {

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source process. */
  def apply[A](source: Stream[Task, TimeStamped[A]], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit S: Strategy, scheduler: ScheduledExecutorService): TimeSeries[Task, A] = {
    val src: TimeSeries[Task, A] = source.map(tsa => tsa.map(Some(_): Option[A]))
    val ticks: TimeSeries[Task, Nothing] = timeTicks(tickPeriod).map(tsu => tsu.map(_ => None))
    src merge ticks pipe TimeStamped.reorderLocally(reorderOver)
  }

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source process. */
  def lift[A](source: Stream[Task, A], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit S: Strategy, scheduler: ScheduledExecutorService): TimeSeries[Task, A] =
    apply(source map TimeStamped.now, tickPeriod, reorderOver)

  /** Stream of time ticks spaced by `tickPeriod`. */
  private def timeTicks(tickPeriod: FiniteDuration = 1.second)(implicit S: Strategy, scheduler: ScheduledExecutorService): Stream[Task, TimeStamped[Unit]] =
    time.awakeEvery(tickPeriod) map { _ => TimeStamped.now(()) }

  /**
   * Stream transducer that converts a stream of timestamped values with monotonically increasing timestamps in
   * to a stream of timestamped ticks or values, where a tick is emitted every `tickPeriod`.
   * Ticks are emitted between values from the source stream.
   */
  def interpolateTicks[A](tickPeriod: FiniteDuration = 1.second): Process1[TimeStamped[A], TimeSeriesValue[A]] = {
    val tickPeriodMillis = tickPeriod.toMillis
    def go(nextTick: Instant): Stream.Handle[Pure, TimeStamped[A]] => Pull[Pure, TimeSeriesValue[A], Stream.Handle[Pure, TimeStamped[A]]] = h => {
      def tickTime(x: Int) = nextTick plusMillis (x * tickPeriodMillis)
      h.receive1 { case tsa #: tl =>
        if (tsa.time.toEpochMilli < nextTick.toEpochMilli) Pull.output1(tsa.toTimeSeriesValue) >> go(nextTick)(tl)
        else {
          val tickCount = ((tsa.time.toEpochMilli - nextTick.toEpochMilli) / tickPeriodMillis + 1).toInt
          val tickTimes = (0 until tickCount) map tickTime
          val ticks = tickTimes map TimeSeriesValue.tick
          Pull.output(Chunk.seq(ticks :+ tsa.toTimeSeriesValue)) >> go(tickTime(tickCount))(tl)
        }
      }
    }
    in => in pull { h =>
      h.receive1 { case tsa #: tl =>
        Pull.output1(tsa.toTimeSeriesValue) >> go(tsa.time plusMillis tickPeriodMillis)(tl)
      }
    }
  }

  /**
   * Combinator that converts a `Process1[A, B]` in to a `TimeSeriesTransducer[Pure, A, B]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserve[A, B](p: Process1[A, B]): TimeSeriesTransducer[Pure, A, B] = {
    def go(stepper: Stepper[A, B]): Stream.Handle[Pure, Option[A]] => Pull[Pure, Option[B], Stream.Handle[Pure, Option[A]]] = h => {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(chunk, next) =>
          Pull.output(chunk.map(Some(_))) >> go(next)(h)
        case Stepper.Await(receive) =>
          h.receive1 { case hd #: tl =>
            hd match {
              case None =>
                Pull.output1(None) >> go(stepper)(h)
              case Some(a) =>
                go(receive(Some(Chunk.singleton(a))))(h)
            }
          }
      }
    }
    TimeStamped.preserveTimeStamps(_ pull go(process1.stepper(p)))
  }

  /**
   * Combinator that converts a `Process1[TimeStamped[A], TimeStamped[B]]` in to a `TimesSeriesTransducer[A, B]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTicks[A, B](p: Process1[TimeStamped[A], TimeStamped[B]]): TimeSeriesTransducer[Pure, A, B] = {
    def go(stepper: Stepper[TimeStamped[A], TimeStamped[B]]): Stream.Handle[Pure, TimeSeriesValue[A]] => Pull[Pure, TimeSeriesValue[B], Stream.Handle[Pure, TimeSeriesValue[A]]] = h => {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(chunk, next) =>
          Pull.output(chunk.map { tsb => tsb.map(Some.apply) }) >> go(next)(h)
        case Stepper.Await(receive) =>
          h.receive1 {
            case (tick @ TimeStamped(_, None)) #: tl =>
              Pull.output1(tick.asInstanceOf[TimeSeriesValue[B]]) >> go(stepper)(tl)
            case TimeStamped(ts, Some(v)) #: tl =>
              go(receive(Some(Chunk.singleton(TimeStamped(ts, v)))))(tl)
          }
      }
    }
    _ pull go(process1.stepper(p))
  }
}
