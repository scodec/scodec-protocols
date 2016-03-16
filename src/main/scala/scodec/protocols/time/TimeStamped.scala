package scodec.protocols
package time

import language.higherKinds

import scala.concurrent.duration._

import fs2._
import fs2.process1.Stepper
import fs2.util.Task

import java.time.Instant
import java.util.concurrent.ScheduledExecutorService

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
   * Combinator that converts a `Process1[A, B]` in to a `Process1[TimeStamped[A], TimeStamped[B]]` such that
   * timestamps are preserved on elements that flow through the stream.
   */
  def preserveTimeStamps[A, B](p: Process1[A, B]): Process1[TimeStamped[A], TimeStamped[B]] = {
    def go(time: Option[Instant], stepper: Stepper[A, B]): Stream.Handle[Pure, TimeStamped[A]] => Pull[Pure, TimeStamped[B], Stream.Handle[Pure, TimeStamped[A]]] = { h =>
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(chunk, next) =>
          time match {
            case Some(ts) => Pull.output(chunk.map { b => TimeStamped(ts, b) }) >> go(time, next)(h)
            case None => go(time, next)(h)
          }
        case Stepper.Await(receive) =>
          h.receive1 { case tsa #: tl => go(Some(tsa.time), receive(Some(Chunk.singleton(tsa.value))))(tl) }
      }
    }

    _ pull go(None, process1.stepper(p))
  }

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a second.
   *
   * For example, the emitted bits per second of a `Stream[Task, ByteVector]` can be calculated
   * using `perSecondRate(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param f function which extracts a feature of `A`
   */
  def perSecondRate[A, B](f: A => B)(zero: B, combine: (B, B) => B): Process1[TimeStamped[A], TimeStamped[B]] =
    rate(1.second)(f)(zero, combine)

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B Either A]` where `B` is an accumulated feature of `A` over a second.
   *
   * Every incoming `A` is echoed to the output.
   *
   * For example, the emitted bits per second of a `Stream[Task, ByteVector]` can be calculated
   * using `perSecondRate(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param f function which extracts a feature of `A`
   * @param zero identity for `combine`
   * @param combine closed function on `B` which forms a monoid with `zero`
   */
  def withPerSecondRate[A, B](f: A => B)(zero: B, combine: (B, B) => B): Process1[TimeStamped[A], TimeStamped[Either[B, A]]] =
    withRate(1.second)(f)(zero, combine)

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * For example, the emitted bits per second of a `Stream[Task, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate
   * @param f function which extracts a feature of `A`
   * @param zero identity for `combine`
   * @param combine closed function on `B` which forms a monoid with `zero`
   */
  def rate[A, B](over: FiniteDuration)(f: A => B)(zero: B, combine: (B, B) => B): Process1[TimeStamped[A], TimeStamped[B]] =
    in => withRate(over)(f)(zero, combine)(in) pipe process1.collect { case TimeStamped(ts, Left(b)) => TimeStamped(ts, b) }

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[Either[B, A]]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * Every incoming `A` is echoed to the output.
   *
   * For example, the emitted bits per second of a `Stream[Task, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate
   * @param f function which extracts a feature of `A`
   * @param zero identity for `combine`
   * @param combine closed function on `B` which forms a monoid with `zero`
   */
  def withRate[A, B](over: FiniteDuration)(f: A => B)(zero: B, combine: (B, B) => B): Process1[TimeStamped[A], TimeStamped[Either[B, A]]] = {
    val overMillis = over.toMillis
    def go(start: Instant, acc: B): Stream.Handle[Pure, TimeStamped[A]] => Pull[Pure, TimeStamped[Either[B, A]], Stream.Handle[Pure, TimeStamped[A]]] = h => {
      val end = start plusMillis overMillis
      Pull.await1Option[Pure, TimeStamped[A]](h).flatMap {
        case Some(tsa #: tl) =>
          if (tsa.time isBefore end) Pull.output1(tsa map Right.apply) >> go(start, combine(acc, f(tsa.value)))(tl)
          else Pull.output1(TimeStamped(end, Left(acc))) >> go(end, zero)(tl.push1(tsa))
        case None =>
          Pull.output1(TimeStamped(end, Left(acc))) >> Pull.done
      }
    }
    _ pull { h =>
      h.receive1 { case tsa #: tl =>
        Pull.output1(tsa.map(Right.apply)) >> go(tsa.time, f(tsa.value))(tl)
      }
    }
  }

  /**
   * Returns a stream that is the throttled version of the source stream.
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
  def throttle[A](source: Stream[Task, TimeStamped[A]], throttlingFactor: Double)(implicit S: Strategy, scheduler: ScheduledExecutorService): Stream[Task, TimeStamped[A]] = {
    import wye._

    val tickDuration = 100.milliseconds
    val ticksPerSecond = 1.second.toMillis / tickDuration.toMillis

    def doThrottle: Wye[Task, TimeStamped[A], Unit, TimeStamped[A]] = {

      type PullFromSourceOrTicks = (Stream.Handle[Task, TimeStamped[A]], Stream.Handle[Task, Unit]) => Pull[Task, TimeStamped[A], (Stream.Handle[Task, TimeStamped[A]], Stream.Handle[Task, Unit])]

      def takeUpto(chunk: Chunk[TimeStamped[A]], upto: Instant): (Chunk[TimeStamped[A]], Chunk[TimeStamped[A]]) = {
        val uptoMillis = upto.toEpochMilli
        val toTake = chunk.indexWhere { _.time.toEpochMilli > uptoMillis }.getOrElse(chunk.size)
        (chunk.take(toTake), chunk.drop(toTake))
      }

      def read(upto: Instant): PullFromSourceOrTicks = { (src, ticks) =>
        src.receive {
          case chunk #: tl =>
            if (chunk.isEmpty) read(upto)(tl, ticks)
            else {
              val (toOutput, pending) = takeUpto(chunk, upto)
              if (pending.isEmpty) Pull.output(toOutput) >> read(upto)(tl, ticks)
              else Pull.output(toOutput) >> awaitTick(upto, pending)(tl, ticks)
            }
        }
      }

      def awaitTick(upto: Instant, pending: Chunk[TimeStamped[A]]): PullFromSourceOrTicks = { (src, ticks) =>
        ticks.receive1 {
          case tick #: tl =>
            val newUpto = upto.plusMillis(((1000 / ticksPerSecond) * throttlingFactor).toLong)
            val (toOutput, stillPending) = takeUpto(pending, newUpto)
            if (stillPending.isEmpty) {
              Pull.output(toOutput) >> read(newUpto)(src, tl)
            } else {
              Pull.output(toOutput) >> awaitTick(newUpto, stillPending)(src, tl)
            }
        }
      }

      _.pull2(_) {
        (src, ticks) => src.await1.flatMap { case tsa #: tl => Pull.output1(tsa) >> read(tsa.time)(tl, ticks) }
      }
    }

    (source pipe2 time.awakeEvery(tickDuration).map(_ => ()))(doThrottle)
  }

  /**
   * Stream transducer that filters the specified timestamped values to ensure
   * the output time stamps are always increasing in time. Other values are
   * dropped.
   */
  def increasing[A]: Process1[TimeStamped[A], TimeStamped[A]] =
    increasingW andThen process1.collect { case Right(out) => out }

  /**
   * Stream transducer that filters the specified timestamped values to ensure
   * the output time stamps are always increasing in time. The increasing values
   * are emitted as output of the writer, while out of order values are written
   * to the writer side of the writer.
   */
  def increasingW[A]: Process1[TimeStamped[A], Either[TimeStamped[A], TimeStamped[A]]] = {
    def notBefore(last: Instant): Stream.Handle[Pure, TimeStamped[A]] => Pull[Pure, Either[TimeStamped[A], TimeStamped[A]], Stream.Handle[Pure, TimeStamped[A]]] = h => {
      h.receive1 {
        case tsa #: tl =>
          val now = tsa.time
          if (last.toEpochMilli <= now.toEpochMilli) Pull.output1(Right(tsa)) >> notBefore(now)(tl)
          else Pull.output1(Left(tsa)) >> notBefore(last)(tl)
      }
    }

    _ pull { h =>
      h.receive1 { case tsa #: tl =>
        Pull.output1(Right(tsa)) >> notBefore(tsa.time)(tl)
      }
    }
  }

  /**
   * Stream transducer that reorders a stream of timestamped values that are mostly ordered,
   * using a time based buffer of the specified duration. See [[attemptReorderLocally]] for details.
   *
   * The resulting process is guaranteed to always emit values in time increasing order.
   * Values may be dropped from the source process if they were not successfully reordered.
   */
  def reorderLocally[A](over: FiniteDuration): Process1[TimeStamped[A], TimeStamped[A]] =
    reorderLocallyW(over) andThen process1.collect { case Right(tsa) => tsa }

  /**
   * Stream transducer that reorders a stream of timestamped values that are mostly ordered,
   * using a time based buffer of the specified duration. See [[attemptReorderLocally]] for details.
   *
   * The resulting process is guaranteed to always emit output values in time increasing order.
   * Any values that could not be reordered due to insufficient buffer space are emitted on the writer (left)
   * side.
   */
  def reorderLocallyW[A](over: FiniteDuration): Process1[TimeStamped[A], Either[TimeStamped[A], TimeStamped[A]]] =
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
   * will be accumulated in to the buffer until the source halts, and then the{
   * values will be emitted in order.
   */
  def attemptReorderLocally[A](over: FiniteDuration): Process1[TimeStamped[A], TimeStamped[A]] = {
    import scala.collection.immutable.SortedMap
    val overMillis = over.toMillis

    def outputMapValues(m: SortedMap[Long, Vector[TimeStamped[A]]]) =
      Pull.output(Chunk.seq(m.foldLeft(Vector.empty[TimeStamped[A]]) { case (acc, (_, tss)) => acc ++ tss }))

    def go(buffered: SortedMap[Long, Vector[TimeStamped[A]]]): Stream.Handle[Pure, TimeStamped[A]] => Pull[Pure, TimeStamped[A], Stream.Handle[Pure, TimeStamped[A]]] = h => {
      Pull.await1Option(h).flatMap {
        case Some(tsa #: tl) =>
          val tsaTimeMillis = tsa.time.toEpochMilli
          val until = tsaTimeMillis - overMillis
          val (toOutput, toBuffer) = buffered span { case (x, _) => x <= until }
          val updatedBuffer = toBuffer + (tsaTimeMillis -> (toBuffer.getOrElse(tsaTimeMillis, Vector.empty[TimeStamped[A]]) :+ tsa))
          outputMapValues(toOutput) >> go(updatedBuffer)(tl)
        case None =>
          outputMapValues(buffered) >> Pull.done
      }
    }

    _ pull go(SortedMap.empty)
  }

  def liftL[A, B, C](p: Process1[TimeStamped[A], TimeStamped[B]]): Process1[TimeStamped[Either[A, C]], TimeStamped[Either[B, C]]] = {
    def go(stepper: Stepper[TimeStamped[A], TimeStamped[B]]): Stream.Handle[Pure, TimeStamped[Either[A, C]]] => Pull[Pure, TimeStamped[Either[B, C]], Stream.Handle[Pure, TimeStamped[Either[A, C]]]] = h => {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(chunk, next) =>
          Pull.output(chunk.map { tsb => tsb.map { b => Left(b): Either[B, C] }}) >> go(next)(h)
        case Stepper.Await(receive) =>
          h.receive {
            case chunk #: tl =>
              chunk.uncons match {
                case None =>
                  go(stepper)(tl)
                case Some((head @ TimeStamped(time, Right(c)), tail)) =>
                  val numHeadRights = {
                    val indexOfFirstLeft = tail.indexWhere(_.value.isLeft)
                    indexOfFirstLeft match {
                      case None => chunk.size
                      case Some(idx) => 1 + idx
                    }
                  }
                  val toOutput = chunk.take(numHeadRights).asInstanceOf[Chunk[TimeStamped[Either[B, C]]]]
                  val remainder = chunk.drop(numHeadRights)
                  Pull.output(toOutput) >> go(stepper)(if (remainder.isEmpty) tl else tl.push(remainder))
                case Some((TimeStamped(time, Left(a)), tail)) =>
                  val numHeadLefts = {
                    val indexOfFirstRight = tail.indexWhere(_.value.isRight)
                    indexOfFirstRight match {
                      case None => chunk.size
                      case Some(idx) => 1 + idx
                    }
                  }
                  val toFeed = chunk.take(numHeadLefts).map { _ map { case Left(a) => a; case Right(_) => sys.error("Chunk is all lefts!") } }
                  val remainder = chunk.drop(numHeadLefts)
                  go(receive(Some(toFeed)))(if (remainder.isEmpty) tl else tl.push(remainder))
              }
          }
      }
    }
    _ pull go(process1.stepper(p))
  }

  def liftR[A, B, C](p: Process1[TimeStamped[A], TimeStamped[B]]): Process1[TimeStamped[Either[C, A]], TimeStamped[Either[C, B]]] = {
    def swap[X, Y]: Process1[TimeStamped[Either[X, Y]], TimeStamped[Either[Y, X]]] =
      process1.lift((_: TimeStamped[Either[X, Y]]).map(_.swap))
    swap[C, A].andThen(liftL(p)).andThen(swap[B, C])
  }
}
