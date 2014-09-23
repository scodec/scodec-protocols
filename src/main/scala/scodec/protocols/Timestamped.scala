package scodec.protocols

import scalaz.{ Lens, LensFamily, Monoid }
import scalaz.syntax.monoid._
import scalaz.stream.{ Process, Process1, process1 }

/** Value timestamped with UTC time. */
case class Timestamped[A](timestamp: Double, value: A)

object Timestamped {

  def now[A](a: A): Timestamped[A] = Timestamped(System.currentTimeMillis / 1000.0, a)

  object Lenses {
    def Timestamp[A]: Lens[Timestamped[A], Double] = Lens.lensu((t, s) => t.copy(timestamp = s), _.timestamp)
    def Value[A]: Lens[Timestamped[A], A] = Lens.lensu((t, a) => t.copy(value = a), _.value)

    def ValueMap[A, B]: LensFamily[Timestamped[A], Timestamped[B], A, B] =
      Lens.lensFamilyu((tsa, b) => Timestamped(tsa.timestamp, b), _.value)
  }

  /**
   * Combinator that converts a `Process1[A, B]` in to a `Process1[Timestamped[A], Timestamped[B]]` such that
   * timestamps are preserved on elements that flow through the process.
   */
  def preserveTimestamps[A, B](p: Process1[A, B]): Process1[Timestamped[A], Timestamped[B]] =
    LensCombinators.lensf(Lenses.ValueMap[A, B])(p)

  /**
   * Stream transducer that converts a stream of `Timestamped[A]` in to a stream of
   * `Timestamped[B]` where `B` is an accumulated feature of `A` over a second.
   *
   * For example, the emitted bits per second of a `Process[Task, ByteVector]` can be calculated
   * using `perSecondRate(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param f function which extracts a feature of `A`
   */
  def perSecondRate[A, B: Monoid](f: A => B): Process1[Timestamped[A], Timestamped[B]] =
    rate(1.0)(f)

  /**
   * Stream transducer that converts a stream of `Timestamped[A]` in to a stream of
   * `Timestamped[B]` where `B` is an accumulated feature of `A` over a specified time period.
   *
   * For example, the emitted bits per second of a `Process[Task, ByteVector]` can be calculated
   * using `rate(1.0)(_.size * 8)`, which yields a stream of the emitted bits per second.
   *
   * @param over time period over which to calculate (in seconds)
   * @param f function which extracts a feature of `A`
   */
  def rate[A, B: Monoid](over: Double)(f: A => B): Process1[Timestamped[A], Timestamped[B]] = {
    def go(start: Double, acc: B): Process1[Timestamped[A], Timestamped[B]] = {
      val end = start + over
      Process.receive1Or[Timestamped[A], Timestamped[B]](Process.emit(Timestamped(start, acc))) {
        case t @ Timestamped(ts, a) =>
          if (ts < end) go(start, acc |+| f(a))
          else Process.emit(Timestamped(start, acc)) ++ process1.feed1(t)(go(end, Monoid[B].zero))
      }
    }
    Process.await1[Timestamped[A]].flatMap { first =>
      go(first.timestamp, f(first.value))
    }
  }
}
