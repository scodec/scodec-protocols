package scodec.protocols
package time

import language.higherKinds

import java.time.Instant

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.effect.Effect

import fs2._

/** Companion for [[TimeSeries]]. */
object TimeSeries {

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source stream. */
  def apply[F[_],A](source: Stream[F, TimeStamped[A]], scheduler: Timer[F], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit F: Effect[F], ec: ExecutionContext): TimeSeries[F, A] = {
    val src: TimeSeries[F, A] = source.map(tsa => tsa.map(Some(_): Option[A]))
    val ticks: TimeSeries[F, Nothing] = timeTicks(scheduler, tickPeriod).map(tsu => tsu.map(_ => None))
    src merge ticks through TimeStamped.reorderLocally(reorderOver)
  }

  /** Stream of either time ticks (spaced by `tickPeriod`) or values from the source stream. */
  def lift[F[_],A](source: Stream[F, A], scheduler: Timer[F], tickPeriod: FiniteDuration = 1.second, reorderOver: FiniteDuration = 100.milliseconds)(implicit F: Effect[F], ec: ExecutionContext): TimeSeries[F, A] =
    apply(source map TimeStamped.now, scheduler, tickPeriod, reorderOver)

  /** Stream of time ticks spaced by `tickPeriod`. */
  private def timeTicks[F[_]](scheduler: Timer[F], tickPeriod: FiniteDuration)(implicit F: Effect[F], ec: ExecutionContext): Stream[F, TimeStamped[Unit]] =
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
          hd.force.splitWhile(_.time.toEpochMilli < nextTick.toEpochMilli) match {
            case Left((_,out)) =>
              (if (out.isEmpty) Pull.pure(()) else Pull.output(fs2.Chunk.catenated(out.map(fs2.Chunk.chunk)).map(_.toTimeSeriesValue))) >> go(nextTick, tl)
            case Right((prefix,suffix)) =>
              val out = if (prefix.isEmpty) Pull.pure(()) else Pull.output(fs2.Chunk.catenated(prefix.map(fs2.Chunk.chunk)).map(_.toTimeSeriesValue))
              // we know suffix is non-empty and suffix.head has a time >= next tick time
              val rest = suffix.take(1).force.uncons1 match {
                case Left(_) => sys.error("not possible; suffix has at least 1 element")
                case Right((next, _)) =>
                  val tickCount = ((next.time.toEpochMilli - nextTick.toEpochMilli) / tickPeriodMillis + 1).toInt
                  val tickTimes = (0 until tickCount) map tickTime
                  val ticks = tickTimes map TimeSeriesValue.tick
                  Pull.output(fs2.Chunk.seq(ticks)) >> go(tickTime(tickCount), tl.cons(suffix))
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
        case TimeStamped(t, Some(Left(lValue))) => l.transform(lState, TimeStamped(t, Some(lValue))).mapResult(_ -> rState)
        case TimeStamped(t, Some(Right(rValue))) => r.transform(rState, TimeStamped(t, Some(rValue))).mapResult(lState -> _)
        case TimeStamped(t, None) =>
          val lSegment = l.transform(lState, TimeStamped(t, None))
          val rSegment = r.transform(rState, TimeStamped(t, None))
          lSegment.append(rSegment)
      }
    }, { case (lState, rState) => l.onComplete(lState) ++ r.onComplete(rState) })
}
