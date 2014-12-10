package scodec

import scalaz.\/
import scalaz.concurrent.Task
import scalaz.stream.{ Process, Process1 }

package object protocols {

  /**
   * A single value in a `TimeSeries`. Provides a timestamp along with either a value of type `A` or
   * a clock tick (represented by a left unit).
   */
  type TimeSeriesValue[+A] = TimeStamped[Unit \/ A]

  /**
   * A stream of timestamped values or clock ticks.
   *
   * Values are represented as right values in a `TimeStamped[Unit \/ A]`, whereas
   * clock ticks are represented as left values. This encoding allows for an indication
   * of time passage with no observed values.
   *
   * Generally, time series appear in increasing order, and many combinators that work with
   * time series will rely on that. For streams that are globally ordered, but not locally ordered,
   * i.e., near adjacent values might be out of order but values at great distance from each other
   * are ordered, consider using `TimeStamped.reorderLocally` to adjust.
   */
  type TimeSeries[+A] = Process[Task, TimeSeriesValue[A]]

  /** Alias for a stream transducer on time series values. */
  type TimeSeriesTransducer[-A, +B] = Process1[TimeSeriesValue[A], TimeSeriesValue[B]]
}
