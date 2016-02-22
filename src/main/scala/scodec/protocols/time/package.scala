package scodec
package protocols

import language.higherKinds

import fs2.Stream

package object time {

  /**
   * A single value in a `TimeSeries`. Provides a timestamp along with either a value of type `A` or
   * a clock tick (represented by a none).
   */
  type TimeSeriesValue[+A] = TimeStamped[Option[A]]

  /**
   * A stream of timestamped values or clock ticks.
   *
   * Values are represented as right values in a `TimeStamped[Option[A]]`, whereas
   * clock ticks are represented as nones. This encoding allows for an indication
   * of time passage with no observed values.
   *
   * Generally, time series appear in increasing order, and many combinators that work with
   * time series will rely on that. For streams that are globally ordered, but not locally ordered,
   * i.e., near adjacent values might be out of order but values at great distance from each other
   * are ordered, consider using `TimeStamped.reorderLocally` to adjust.
   */
  type TimeSeries[F[_], +A] = Stream[F, TimeSeriesValue[A]]

  /** Alias for a stream transducer on time series values. */
  type TimeSeriesTransducer[F[_], -A, +B] = Stream[F, TimeSeriesValue[A]] => Stream[F, TimeSeriesValue[B]]
}
