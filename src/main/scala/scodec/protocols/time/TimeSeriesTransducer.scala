package scodec.protocols
package time

import language.higherKinds

import fs2._
import fs2.pipe.Stepper
import java.time.Instant

import pipes._

/** Companion for [[TimeSeriesTransducer]]. */
object TimeSeriesTransducer {

  def lift[F[_], A, B](f: A => B): TimeSeriesTransducer[F, A, B] =
    pipe.lift { _ map { _ map f } }

  def either[L, R, O](left: TimeSeriesTransducer[Pure, L, O], right: TimeSeriesTransducer[Pure, R, O]): TimeSeriesTransducer[Pure, Either[L, R], O] = {

    type ThisPull = Pull[Pure, TimeSeriesValue[O], Stream.Handle[Pure, TimeSeriesValue[Either[L, R]]]]

    def gatherBoth(
      left: Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
      right: Stepper[TimeSeriesValue[R], TimeSeriesValue[O]]
    )(
      cont: (Option[Chunk[TimeSeriesValue[L]]] => Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
             Option[Chunk[TimeSeriesValue[R]]] => Stepper[TimeSeriesValue[R], TimeSeriesValue[O]]) => ThisPull
    ): ThisPull = {
      left.stepToAwait { (outL, l) =>
        right.stepToAwait { (outR, r) =>
          var seenTicks: Set[Instant] = Set.empty
          val out = (outL ++ outR).sortBy { _.time.toEpochMilli }.filter {
            case TimeStamped(time, Some(_)) => true
            case TimeStamped(time, None) if seenTicks contains time => false
            case TimeStamped(time, None) =>
              seenTicks += time
              true
          }
          Pull.output(Chunk.indexedSeq(out)) >> cont(l, r)
        }
      }
    }

    def go(
      leftAwait: Option[Chunk[TimeSeriesValue[L]]] =>Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
      rightAwait: Option[Chunk[TimeSeriesValue[R]]] => Stepper[TimeSeriesValue[R], TimeSeriesValue[O]]
    ): Stream.Handle[Pure, TimeSeriesValue[Either[L, R]]] => ThisPull = h => {
      h.receive1 {
        case value #: next =>
          value match {
            case TimeStamped(ts, Some(Left(l))) =>
              leftAwait(Some(Chunk.singleton(TimeStamped(ts, Some(l))))).stepToAwait { (out, newLeftAwait) =>
                Pull.output(Chunk.indexedSeq(out)) >> go(newLeftAwait, rightAwait)(next)
              }
            case TimeStamped(ts, Some(Right(r))) =>
              rightAwait(Some(Chunk.singleton(TimeStamped(ts, Some(r))))).stepToAwait { (out, newRightAwait) =>
                Pull.output(Chunk.indexedSeq(out)) >> go(leftAwait, newRightAwait)(next)
              }
            case TimeStamped(ts, None) =>
              gatherBoth(
                leftAwait(Some(Chunk.singleton(value.asInstanceOf[TimeSeriesValue[L]]))),
                rightAwait(Some(Chunk.singleton(value.asInstanceOf[TimeSeriesValue[R]])))
              ) { case (l, r) => go(l, r)(next) }
          }

      }
    }

    _ pull { h =>
      gatherBoth(pipe.stepper(left), pipe.stepper(right)) { case (l, r) => go(l, r)(h) }
    }
  }

  def drainRight[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], L] = pipe.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[L]]
    case TimeStamped(ts, Some(Left(l))) => TimeStamped(ts, Some(l))
  }

  def drainLeft[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], R] = pipe.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[R]]
    case TimeStamped(ts, Some(Right(r))) => TimeStamped(ts, Some(r))
  }
}
