package scodec.protocols
package time

import language.higherKinds

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.effect.Effect

import fs2._

import java.time.Instant

/** Wrapper that associates a time with a value. */
case class TimeStamped[+A](time: Instant, value: A) {
  def map[B](f: A => B): TimeStamped[B] = copy(value = f(value))
  def mapTime(f: Instant => Instant): TimeStamped[A] = copy(time = f(time))

  def toTimeSeriesValue: TimeSeriesValue[A] = map(Some.apply)
}

object TimeStamped {

  def now[A](a: A): TimeStamped[A] = TimeStamped(Instant.now(), a)

  /** Orders values by timestamp -- values with the same timestamp are considered equal. */
  def timeBasedOrdering[A]: Ordering[TimeStamped[A]] = new Ordering[TimeStamped[A]] {
    def compare(x: TimeStamped[A], y: TimeStamped[A]) = x.time compareTo y.time
  }

  /** Orders values by timestamp, then by value. */
  implicit def ordering[A](implicit A: Ordering[A]): Ordering[TimeStamped[A]] = new Ordering[TimeStamped[A]] {
    def compare(x: TimeStamped[A], y: TimeStamped[A]) = x.time compareTo y.time match {
      case 0 => A.compare(x.value, y.value)
      case other => other
    }
  }

  /**
   * Combinator that converts a `Transform[A, B]` in to a `Transform[TimeStamped[A], TimeStamped[B]]` such that
   * timestamps are preserved on elements that flow through the stream.
   */
  def preserve[I, O](t: Transform[I, O]): Transform.Aux[t.S, TimeStamped[I], TimeStamped[O]] =
    t.lens(_.value, (tsi, o) => tsi.copy(value = o))

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a second.
   *
   * For example, the emitted bits per second of a `Stream[F, ByteVector]` can be calculated
   * using `perSecondRate(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param f function which extracts a feature of `A`
   */
  def perSecondRate[F[_], A, B](f: A => B)(zero: B, combine: (B, B) => B): Pipe[F, TimeStamped[A], TimeStamped[B]] =
    rate(1.second)(f)(zero, combine)

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B Either A]` where `B` is an accumulated feature of `A` over a second.
   *
   * Every incoming `A` is echoed to the output.
   *
   * For example, the emitted bits per second of a `Stream[F, ByteVector]` can be calculated
   * using `perSecondRate(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param f function which extracts a feature of `A`
   * @param zero identity for `combine`
   * @param combine closed function on `B` which forms a monoid with `zero`
   */
  def withPerSecondRate[F[_], A, B](f: A => B)(zero: B, combine: (B, B) => B): Pipe[F, TimeStamped[A], TimeStamped[Either[B, A]]] =
    withRate(1.second)(f)(zero, combine)

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * For example, the emitted bits per second of a `Stream[F, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate
   * @param f function which extracts a feature of `A`
   * @param zero identity for `combine`
   * @param combine closed function on `B` which forms a monoid with `zero`
   */
  def rate[F[_], A, B](over: FiniteDuration)(f: A => B)(zero: B, combine: (B, B) => B): Pipe[F, TimeStamped[A], TimeStamped[B]] =
    in => withRate(over)(f)(zero, combine)(in).collect { case TimeStamped(ts, Left(b)) => TimeStamped(ts, b) }

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[Either[B, A]]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * Every incoming `A` is echoed to the output.
   *
   * For example, the emitted bits per second of a `Stream[F, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate
   * @param f function which extracts a feature of `A`
   * @param zero identity for `combine`
   * @param combine closed function on `B` which forms a monoid with `zero`
   */
  def withRate[F[_], A, B](over: FiniteDuration)(f: A => B)(zero: B, combine: (B, B) => B): Pipe[F, TimeStamped[A], TimeStamped[Either[B, A]]] = {
    val overMillis = over.toMillis
    def go(start: Instant, acc: B, s: Stream[F, TimeStamped[A]]): Pull[F, TimeStamped[Either[B, A]], Unit] = {
      val end = start plusMillis overMillis
      s.pull.uncons1.flatMap {
        case Some((tsa, tl)) =>
          if (tsa.time isBefore end) Pull.output1(tsa map Right.apply) >> go(start, combine(acc, f(tsa.value)), tl)
          else Pull.output1(TimeStamped(end, Left(acc))) >> go(end, zero, tl.cons1(tsa))
        case None =>
          Pull.output1(TimeStamped(end, Left(acc)))
      }
    }
    in => in.pull.uncons1.flatMap {
      case Some((tsa, tl)) => Pull.output1(tsa.map(Right.apply)) >> go(tsa.time, f(tsa.value), tl)
      case None => Pull.done
    }.stream
  }

  /**
   * Returns a stream that is the throttled version of the source stream.
   *
   * Given two adjacent items from the source stream, `a` and `b`, where `a` is emitted
   * first and `b` is emitted second, their time delta is `b.time - a.time`.
   *
   * This function creates a stream that emits values at wall clock times such that
   * the time delta between any two adjacent values is proportional to their time delta
   * in the source stream.
   *
   * The `throttlingFactor` is a scaling factor that determines how much source time a unit
   * of wall clock time is worth. A value of 1.0 causes the output stream to emit
   * values spaced in wall clock time equal to their time deltas. A value of 2.0
   * emits values at twice the speed of wall clock time.
   *
   * This is particularly useful when timestamped data can be read in bulk (e.g., from a capture file)
   * but should be "played back" at real time speeds.
   */
  def throttle[F[_], A](source: Stream[F, TimeStamped[A]], throttlingFactor: Double, tickResolution: FiniteDuration = 100.milliseconds)(implicit F: Effect[F], ec: ExecutionContext, scheduler: Scheduler): Stream[F, TimeStamped[A]] = {

    val ticksPerSecond = 1.second.toMillis / tickResolution.toMillis

    def doThrottle: Pipe2[F, TimeStamped[A], Unit, TimeStamped[A]] = {

      type PullFromSourceOrTicks = (Stream[F, TimeStamped[A]], Stream[F, Unit]) => Pull[F, TimeStamped[A], Unit]

      def takeUpto(chunk: Chunk[TimeStamped[A]], upto: Instant): (Chunk[TimeStamped[A]], Chunk[TimeStamped[A]]) = {
        val uptoMillis = upto.toEpochMilli
        val toTake = chunk.indexWhere { _.time.toEpochMilli > uptoMillis }.getOrElse(chunk.size)
        chunk.strict.splitAt(toTake)
      }

      def read(upto: Instant): PullFromSourceOrTicks = { (src, ticks) =>
        src.pull.unconsChunk.flatMap {
          case Some((chunk, tl)) =>
            if (chunk.isEmpty) read(upto)(tl, ticks)
            else {
              val (toOutput, pending) = takeUpto(chunk, upto)
              if (pending.isEmpty) Pull.output(toOutput) >> read(upto)(tl, ticks)
              else Pull.output(toOutput) >> awaitTick(upto, pending)(tl, ticks)
            }
          case None => Pull.done
        }
      }

      def awaitTick(upto: Instant, pending: Chunk[TimeStamped[A]]): PullFromSourceOrTicks = { (src, ticks) =>
        ticks.pull.uncons1.flatMap {
          case Some((tick, tl)) =>
            val newUpto = upto.plusMillis(((1000 / ticksPerSecond) * throttlingFactor).toLong)
            val (toOutput, stillPending) = takeUpto(pending, newUpto)
            if (stillPending.isEmpty) {
              Pull.output(toOutput) >> read(newUpto)(src, tl)
            } else {
              Pull.output(toOutput) >> awaitTick(newUpto, stillPending)(src, tl)
            }
          case None => Pull.done
        }
      }

      (src, ticks) => src.pull.uncons1.flatMap {
        case Some((tsa, tl)) => Pull.output1(tsa) >> read(tsa.time)(tl, ticks)
        case None => Pull.done
      }.stream
    }

    (source through2 time.awakeEvery[F](tickResolution).as(()))(doThrottle)
  }

  /**
   * Stream transducer that filters the specified timestamped values to ensure
   * the output time stamps are always increasing in time. Other values are
   * dropped.
   */
  def increasing[F[_], A]: Pipe[F, TimeStamped[A], TimeStamped[A]] =
    increasingW.andThen(_.collect { case Right(out) => out })

  /**
   * Stream transducer that filters the specified timestamped values to ensure
   * the output time stamps are always increasing in time. The increasing values
   * are emitted as output of the writer, while out of order values are written
   * to the writer side of the writer.
   */
  def increasingW[F[_], A]: Pipe[F, TimeStamped[A], Either[TimeStamped[A], TimeStamped[A]]] = {
    _.scanSegments(Long.MinValue) { (last, segment) =>
      segment.mapAccumulate(last) { (last, tsa) =>
        val now = tsa.time.toEpochMilli
        if (last <= now) (now, Right(tsa)) else (last, Left(tsa))
      }.mapResult { case (_, last) => last }
    }
  }

  /**
   * Stream transducer that reorders a stream of timestamped values that are mostly ordered,
   * using a time based buffer of the specified duration. See [[attemptReorderLocally]] for details.
   *
   * The resulting stream is guaranteed to always emit values in time increasing order.
   * Values may be dropped from the source stream if they were not successfully reordered.
   */
  def reorderLocally[F[_], A](over: FiniteDuration): Pipe[F, TimeStamped[A], TimeStamped[A]] =
    reorderLocallyW(over).andThen(_.collect { case Right(tsa) => tsa })

  /**
   * Stream transducer that reorders a stream of timestamped values that are mostly ordered,
   * using a time based buffer of the specified duration. See [[attemptReorderLocally]] for details.
   *
   * The resulting stream is guaranteed to always emit output values in time increasing order.
   * Any values that could not be reordered due to insufficient buffer space are emitted on the writer (left)
   * side.
   */
  def reorderLocallyW[F[_], A](over: FiniteDuration): Pipe[F, TimeStamped[A], Either[TimeStamped[A], TimeStamped[A]]] =
    attemptReorderLocally(over) andThen increasingW

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
   * will be accumulated in to the buffer until the source halts, and then the
   * values will be emitted in order.
   */
  def attemptReorderLocally[F[_], A](over: FiniteDuration): Pipe[F, TimeStamped[A], TimeStamped[A]] = {
    import scala.collection.immutable.SortedMap
    val overMillis = over.toMillis

    def outputMapValues(m: SortedMap[Long, Vector[TimeStamped[A]]]) =
      Pull.output(Chunk.seq(m.foldLeft(Vector.empty[TimeStamped[A]]) { case (acc, (_, tss)) => acc ++ tss }))

    def go(buffered: SortedMap[Long, Vector[TimeStamped[A]]], s: Stream[F, TimeStamped[A]]): Pull[F, TimeStamped[A], Unit] = {
      s.pull.unconsChunk.flatMap {
        case Some((hd, tl)) =>
          val all = hd.toVector.foldLeft(buffered) { (acc, tsa) =>
            val k = tsa.time.toEpochMilli
            acc.updated(k, acc.getOrElse(k, Vector.empty) :+ tsa)
          }
          if (all.isEmpty) go(buffered, tl)
          else {
            val until = all.last._2.head.time.toEpochMilli - overMillis
            val (toOutput, toBuffer) = all span { case (x, _) => x <= until }
            outputMapValues(toOutput) >> go(toBuffer, tl)
          }
        case None =>
          outputMapValues(buffered)
      }
    }

    in => go(SortedMap.empty, in).stream
  }

  def left[I,O,A](t: Transform[TimeStamped[I], TimeStamped[O]]): Transform.Aux[t.S, TimeStamped[Either[I,A]], TimeStamped[Either[O,A]]] =
    t.semilens({
      case TimeStamped(t, Left(i)) => Right(TimeStamped(t, i))
      case TimeStamped(t, Right(a)) => Left(TimeStamped(t, Right(a)))
    }, (tse, tso) => tso.map(Left(_)))

  def right[I,O,A](t: Transform[TimeStamped[I], TimeStamped[O]]): Transform.Aux[t.S, TimeStamped[Either[A,I]], TimeStamped[Either[A,O]]] =
    t.semilens({
      case TimeStamped(t, Right(i)) => Right(TimeStamped(t, i))
      case TimeStamped(t, Left(a)) => Left(TimeStamped(t, Left(a)))
    }, (tse, tso) => tso.map(Right(_)))
}
