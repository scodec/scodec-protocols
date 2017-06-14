package scodec.protocols
package time

import language.higherKinds

import fs2._
import fs2.Pipe.Stepper
import java.time.Instant

import pipes._

/** Companion for [[TimeSeriesTransducer]]. */
object TimeSeriesTransducer {

  def lift[F[_], A, B](f: A => B): TimeSeriesTransducer[F, A, B] =
    _.map(_.map(_.map(f)))

  def either[L, R, O](left: TimeSeriesTransducer[Pure, L, O], right: TimeSeriesTransducer[Pure, R, O]): TimeSeriesTransducer[Pure, Either[L, R], O] = {

    type ThisPull = Pull[Pure, TimeSeriesValue[O], Unit]

    def gatherBoth(
      left: Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
      right: Stepper[TimeSeriesValue[R], TimeSeriesValue[O]]
    )(
      cont: (Option[Segment[TimeSeriesValue[L],Unit]] => Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
             Option[Segment[TimeSeriesValue[R],Unit]] => Stepper[TimeSeriesValue[R], TimeSeriesValue[O]]) => ThisPull
    ): ThisPull = {
      left.stepToAwait { (outL, l) =>
        right.stepToAwait { (outR, r) =>
          var seenTicks: Set[Instant] = Set.empty
          val out = (outL ++ outR).toVector.sortBy { _.time.toEpochMilli }.filter {
            case TimeStamped(time, Some(_)) => true
            case TimeStamped(time, None) if seenTicks contains time => false
            case TimeStamped(time, None) =>
              seenTicks += time
              true
          }
          Pull.output(Chunk.vector(out)) >> cont(l, r)
        }
      }
    }

    def go(
      leftAwait: Option[Segment[TimeSeriesValue[L],Unit]] => Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
      rightAwait: Option[Segment[TimeSeriesValue[R],Unit]] => Stepper[TimeSeriesValue[R], TimeSeriesValue[O]],
      s: Stream[Pure, TimeSeriesValue[Either[L, R]]]
    ): ThisPull = {
      s.pull.uncons1.flatMap {
        case Some((value, next)) =>
          value match {
            case TimeStamped(ts, Some(Left(l))) =>
              leftAwait(Some(Chunk.singleton(TimeStamped(ts, Some(l))))).stepToAwait { (out, newLeftAwait) =>
                Pull.output(out) >> go(newLeftAwait, rightAwait, next)
              }
            case TimeStamped(ts, Some(Right(r))) =>
              rightAwait(Some(Chunk.singleton(TimeStamped(ts, Some(r))))).stepToAwait { (out, newRightAwait) =>
                Pull.output(out) >> go(leftAwait, newRightAwait, next)
              }
            case TimeStamped(ts, None) =>
              gatherBoth(
                leftAwait(Some(Chunk.singleton(value.asInstanceOf[TimeSeriesValue[L]]))),
                rightAwait(Some(Chunk.singleton(value.asInstanceOf[TimeSeriesValue[R]])))
              ) { case (l, r) => go(l, r, next) }
          }
        case None => Pull.done
      }
    }

    in => gatherBoth(Pipe.stepper(left), Pipe.stepper(right)) { case (l, r) => go(l, r, in) }.stream
  }

  def drainRight[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], L] = _.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[L]]
    case TimeStamped(ts, Some(Left(l))) => TimeStamped(ts, Some(l))
  }

  def drainLeft[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], R] = _.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[R]]
    case TimeStamped(ts, Some(Right(r))) => TimeStamped(ts, Some(r))
  }
}
