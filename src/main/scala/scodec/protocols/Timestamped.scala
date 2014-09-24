package scodec.protocols

import scalaz.{ Lens, LensFamily, Monoid }
import scalaz.syntax.monoid._
import scalaz.stream.{ Process, Process1, process1 }

/** Value timestamped with UTC time. */
case class TimeStamped[A](time: Double, value: A)

object TimeStamped {

  def now[A](a: A): TimeStamped[A] = TimeStamped(System.currentTimeMillis / 1000.0, a)

  object Lenses {
    def Timestamp[A]: Lens[TimeStamped[A], Double] = Lens.lensu((t, s) => t.copy(time = s), _.time)
    def Value[A]: Lens[TimeStamped[A], A] = Lens.lensu((t, a) => t.copy(value = a), _.value)

    def ValueMap[A, B]: LensFamily[TimeStamped[A], TimeStamped[B], A, B] =
      Lens.lensFamilyu((tsa, b) => TimeStamped(tsa.time, b), _.value)
  }

  /**
   * Combinator that converts a `Process1[A, B]` in to a `Process1[TimeStamped[A], TimeStamped[B]]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTimeStamps[A, B](p: Process1[A, B]): Process1[TimeStamped[A], TimeStamped[B]] =
    process1ext.lensf(Lenses.ValueMap[A, B])(p)

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
    rate(1.0)(f)

  /**
   * Stream transducer that converts a stream of `TimeStamped[A]` in to a stream of
   * `TimeStamped[B]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * For example, the emitted bits per second of a `Process[Task, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate (in seconds)
   * @param f function which extracts a feature of `A`
   */
  def rate[A, B: Monoid](over: Double)(f: A => B): Process1[TimeStamped[A], TimeStamped[B]] = {
    def go(start: Double, acc: B): Process1[TimeStamped[A], TimeStamped[B]] = {
      val end = start + over
      Process.receive1Or[TimeStamped[A], TimeStamped[B]](Process.emit(TimeStamped(start, acc))) {
        case t @ TimeStamped(time, a) =>
          if (time < end) go(start, acc |+| f(a))
          else Process.emit(TimeStamped(start, acc)) ++ process1.feed1(t)(go(end, Monoid[B].zero))
      }
    }
    Process.await1[TimeStamped[A]].flatMap { first =>
      go(first.time, f(first.value))
    }
  }
}
