package scodec.protocols

import scalaz.{ Lens, LensFamily }
import scalaz.stream.Process1

/** Value timestamped with UTC time. */
case class Timestamped[A](timestamp: Double, value: A)

object Timestamped {
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
}
