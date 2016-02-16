package scodec.protocols
package time

import fs2._
import fs2.process1.Stepper
import java.time.Instant

import process1ext._

/** Companion for [[TimeSeriesTransducer]]. */
object TimeSeriesTransducer {

  def lift[A, B](f: A => B): TimeSeriesTransducer[A, B] =
    process1.lift { _ map { _ map f } }

  def either[L, R, O](left: TimeSeriesTransducer[L, O], right: TimeSeriesTransducer[R, O]): TimeSeriesTransducer[Either[L, R], O] = {

    type ThisPull = Pull[Pure, TimeSeriesValue[O], Stream.Handle[Pure, TimeSeriesValue[Either[L, R]]]]

    def gatherBoth(
      left: Stepper[TimeSeriesValue[L], TimeSeriesValue[O]],
      right: Stepper[TimeSeriesValue[R], TimeSeriesValue[O]]
    )(
      cont: (Stepper.Await[TimeSeriesValue[L], TimeSeriesValue[O]], Stepper.Await[TimeSeriesValue[R], TimeSeriesValue[O]]) => ThisPull
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
      leftAwait: Stepper.Await[TimeSeriesValue[L], TimeSeriesValue[O]],
      rightAwait: Stepper.Await[TimeSeriesValue[R], TimeSeriesValue[O]]
    ): Stream.Handle[Pure, TimeSeriesValue[Either[L, R]]] => ThisPull = h => {
      h.receive1 {
        case value #: next =>
          value match {
            case TimeStamped(ts, Some(Left(l))) =>
              leftAwait.receive(Some(Chunk.singleton(TimeStamped(ts, Some(l))))).stepToAwait { (out, newLeftAwait) =>
                Pull.output(Chunk.indexedSeq(out)) >> go(newLeftAwait, rightAwait)(next)
              }
            case TimeStamped(ts, Some(Right(r))) =>
              rightAwait.receive(Some(Chunk.singleton(TimeStamped(ts, Some(r))))).stepToAwait { (out, newRightAwait) =>
                Pull.output(Chunk.indexedSeq(out)) >> go(leftAwait, newRightAwait)(next)
              }
            case TimeStamped(ts, None) =>
              gatherBoth(
                leftAwait.receive(Some(Chunk.singleton(value.asInstanceOf[TimeSeriesValue[L]]))),
                rightAwait.receive(Some(Chunk.singleton(value.asInstanceOf[TimeSeriesValue[R]])))
              ) { case (l, r) => go(l, r)(next) }
          }

      }
    }

    _ pull { h =>
      gatherBoth(process1.stepper(left), process1.stepper(right)) { case (l, r) => go(l, r)(h) }
    }
  }

  def drainRight[L, R]: TimeSeriesTransducer[Either[L, R], L] = process1.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[L]]
    case TimeStamped(ts, Some(Left(l))) => TimeStamped(ts, Some(l))
  }

  def drainLeft[L, R]: TimeSeriesTransducer[Either[L, R], R] = process1.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[R]]
    case TimeStamped(ts, Some(Right(r))) => TimeStamped(ts, Some(r))
  }
}
