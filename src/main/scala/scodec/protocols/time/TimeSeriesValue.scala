package scodec.protocols
package time

import java.time.Instant

/** Companion for [[TimeSeriesValue]]. */
object TimeSeriesValue {
  def apply[A](time: Instant, value: A): TimeSeriesValue[A] = TimeStamped(time, Some(value))
  def tick(time: Instant): TimeSeriesValue[Nothing] = TimeStamped(time, None)
  def now[A](value: A): TimeSeriesValue[A] = TimeStamped.now(Some(value))
  def nowTick: TimeSeriesValue[Nothing] = TimeStamped.now(None)
  def lift[A](t: TimeStamped[A]): TimeSeriesValue[A] = t map Some.apply
}
