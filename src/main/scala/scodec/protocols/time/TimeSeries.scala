package scodec.protocols
package time

import language.higherKinds

import java.time.Instant

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.effect.Effect

import fs2._
import fs2.Pipe.Stepper

/** Companion for [[TimeSeries]]. */
object TimeSeries {

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source stream. */
  def apply[F[_],A](source: Stream[F, TimeStamped[A]], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit F: Effect[F], ec: ExecutionContext, scheduler: Scheduler): TimeSeries[F, A] = {
    val src: TimeSeries[F, A] = source.map(tsa => tsa.map(Some(_): Option[A]))
    val ticks: TimeSeries[F, Nothing] = timeTicks(tickPeriod).map(tsu => tsu.map(_ => None))
    src merge ticks through TimeStamped.reorderLocally(reorderOver)
  }

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source stream. */
  def lift[F[_],A](source: Stream[F, A], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit F: Effect[F], ec: ExecutionContext, scheduler: Scheduler): TimeSeries[F, A] =
    apply(source map TimeStamped.now, tickPeriod, reorderOver)

  /** Stream of time ticks spaced by `tickPeriod`. */
  private def timeTicks[F[_]](tickPeriod: FiniteDuration = 1.second)(implicit F: Effect[F], ec: ExecutionContext, scheduler: Scheduler): Stream[F, TimeStamped[Unit]] =
    time.awakeEvery[F](tickPeriod) map { _ => TimeStamped.now(()) }

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
          hd.splitWhile(_.time.toEpochMilli < nextTick.toEpochMilli) match {
            case Left((_,out)) =>
              (if (out.isEmpty) Pull.pure(()) else Pull.output(Segment.catenated(out).map(_.toTimeSeriesValue))) >> go(nextTick, tl)
            case Right((prefix,suffix)) =>
              val out = if (prefix.isEmpty) Pull.pure(()) else Pull.output(Segment.catenated(prefix).map(_.toTimeSeriesValue))
              // we know suffix is non-empty and suffix.head has a time >= next tick time
              val rest = suffix.take(1).uncons1 match {
                case Left(_) => sys.error("not possible; suffix has at least 1 element")
                case Right((next, _)) =>
                  val tickCount = ((next.time.toEpochMilli - nextTick.toEpochMilli) / tickPeriodMillis + 1).toInt
                  val tickTimes = (0 until tickCount) map tickTime
                  val ticks = tickTimes map TimeSeriesValue.tick
                  Pull.output(Segment.seq(ticks)) >> go(tickTime(tickCount), tl.cons(suffix))
              }
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
   * Combinator that converts a `Pipe[Pure, A, B]` in to a `TimeSeriesTransducer[Pure, A, B]` such that
   * timestamps are preserved on elements that flow through the stream.
   */
  def preserve[A, B](p: Pipe[Pure, A, B]): TimeSeriesTransducer[Pure, A, B] = {
    def go(stepper: Stepper[A, B], s: Stream[Pure, Option[A]]): Pull[Pure, Option[B], Unit] = {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(segment, next) =>
          Pull.output(segment.map(Some(_))) >> go(next, s)
        case Stepper.Await(receive) =>
          s.pull.uncons1.flatMap {
            case Some((hd, tl)) =>
              hd match {
                case None =>
                  Pull.output1(None) >> go(stepper, tl)
                case Some(a) =>
                  go(receive(Some(Chunk.singleton(a))), tl)
              }
            case None => Pull.done
          }
      }
    }
    TimeStamped.preserveTimeStamps(in => go(Pipe.stepper(p), in).stream)
  }

  /**
   * Combinator that converts a `Pipe[Pure, TimeStamped[A], TimeStamped[B]]` in to a `TimesSeriesTransducer[Pure, A, B]` such that
   * timestamps are preserved on elements that flow through the stream.
   */
  def preserveTicks[A, B](p: Pipe[Pure, TimeStamped[A], TimeStamped[B]]): TimeSeriesTransducer[Pure, A, B] = {
    def go(stepper: Stepper[TimeStamped[A], TimeStamped[B]], s: Stream[Pure, TimeSeriesValue[A]]): Pull[Pure, TimeSeriesValue[B], Unit] = {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(segment, next) =>
          Pull.output(segment.map { tsb => tsb.map(Some.apply) }) >> go(next, s)
        case Stepper.Await(receive) =>
          s.pull.uncons1.flatMap {
            case Some(((tick @ TimeStamped(_, None)), tl)) =>
              Pull.output1(tick.asInstanceOf[TimeSeriesValue[B]]) >> go(stepper, tl)
            case Some((TimeStamped(ts, Some(v)), tl)) =>
              go(receive(Some(Chunk.singleton(TimeStamped(ts, v)))), tl)
            case None => Pull.done
          }
      }
    }
    in => go(Pipe.stepper(p), in).stream
  }
}
