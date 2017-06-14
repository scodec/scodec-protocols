package scodec
package protocols
package mpeg
package transport

import scodec.Codec
import scodec.codecs.uint

import fs2._
import fs2.Pipe.Stepper

case class Pid(value: Int) {
  require(value >= Pid.MinValue && value <= Pid.MaxValue)
}

object Pid {
  val MinValue = 0
  val MaxValue = 8191

  implicit val codec: Codec[Pid] = uint(13).as[Pid]
}

case class PidStamped[+A](pid: Pid, value: A) {
  def map[B](f: A => B): PidStamped[B] = copy(value = f(value))
}

object PidStamped {

  /**
   * Combinator that converts a `Pipe[Pure, A, B]` in to a `Pipe[Pure, PidStamped[A], PidStamped[B]]` such that
   * pidstamps are preserved on elements that flow through the process.
   */
  def preservePidStamps[A, B](p: Pipe[Pure, A, B]): Pipe[Pure, PidStamped[A], PidStamped[B]] = {
    def go(pid: Option[Pid], stepper: Stepper[A, B], s: Stream[Pure, PidStamped[A]]): Pull[Pure, PidStamped[B], Unit] = {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(segment, next) =>
          pid match {
            case Some(p) => Pull.output(segment.map { b => PidStamped(p, b) }) >> go(pid, next, s)
            case None => go(pid, next, s)
          }
        case Stepper.Await(receive) =>
          s.pull.uncons1.flatMap {
            case Some((psa, tl)) => go(Some(psa.pid), receive(Some(Chunk.singleton(psa.value))), tl)
            case None => Pull.done
          }
      }
    }

    in => go(None, Pipe.stepper(p), in).stream
  }
}
