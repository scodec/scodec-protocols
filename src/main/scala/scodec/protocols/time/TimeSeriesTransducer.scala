package scodec.protocols
package time

import language.higherKinds

/** Companion for [[TimeSeriesTransducer]]. */
object TimeSeriesTransducer {

  def lift[F[_], A, B](f: A => B): TimeSeriesTransducer[F, A, B] =
    _.map(_.map(_.map(f)))

  def drainRight[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], L] = _.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[L]]
    case TimeStamped(ts, Some(Left(l))) => TimeStamped(ts, Some(l))
  }

  def drainLeft[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], R] = _.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[R]]
    case TimeStamped(ts, Some(Right(r))) => TimeStamped(ts, Some(r))
  }
}
