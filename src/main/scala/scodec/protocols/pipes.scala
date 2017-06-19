package scodec.protocols

import fs2._
import fs2.Pipe.Stepper

/** General purpose combinators for working with pipes that are not included in fs2. */
object pipes { outer =>

  implicit class PipeOps[A, B](val self: Pipe[Pure, A, B]) extends AnyVal {
    def conditionallyFeed[X](f: X => Either[A, B]): Pipe[Pure, X, B] = outer.conditionallyFeed(self, f)
    def liftL[C]: Pipe[Pure, Either[A, C], Either[B, C]] = outer.liftL[A, B, C](self)
    def liftR[C]: Pipe[Pure, Either[C, A], Either[C, B]] = outer.liftR[A, B, C](self)
  }

  /**
   * Accepts values of type `X` and converts them to an `A` or `B`. If `A`, the `A`
   * is fed to `p`. If `B`, the `B` is emitted directly.
   */
  def conditionallyFeed[A, B, X](p: Pipe[Pure, A, B], f: X => Either[A, B]): Pipe[Pure, X, B] =
    in => liftL(p)(in.map(f)).map(_.fold(identity,identity))

  def liftL[A, B, C](p: Pipe[Pure, A, B]): Pipe[Pure, Either[A, C], Either[B, C]] = {
    def go(stepper: Stepper[A, B], s: Stream[Pure, Either[A, C]]): Pull[Pure, Either[B, C], Unit] = {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(chunk, next) =>
          Pull.output(chunk.map { b => Left(b): Either[B, C] }) >> go(next, s)
        case Stepper.Await(receive) =>
          s.pull.unconsChunk.flatMap {
            case None => Pull.done
            case Some((hd,tl)) =>
              hd.uncons1 match {
                case Left(_) =>
                  go(stepper, tl)
                case Right((head @ Right(c), tail)) =>
                  val numHeadRights = {
                    val indexOfFirstLeft = tail.toChunk.indexWhere(_.isLeft)
                    indexOfFirstLeft match {
                      case None => hd.size
                      case Some(idx) => 1 + idx
                    }
                  }
                  val (toOutput, suffix) = hd.strict.splitAt(numHeadRights)
                  Pull.output(toOutput.asInstanceOf[Chunk[Either[B, C]]]) >> go(stepper, if (suffix.isEmpty) tl else tl.cons(suffix))
                case Right((Left(a), tail)) =>
                  val numHeadLefts = {
                    val indexOfFirstRight = tail.toChunk.indexWhere(_.isRight)
                    indexOfFirstRight match {
                      case None => hd.size
                      case Some(idx) => 1 + idx
                    }
                  }
                  val (prefix, suffix) = hd.strict.splitAt(numHeadLefts)
                  val toFeed = prefix.map { case Left(a) => a; case Right(_) => sys.error("Chunk is all lefts!") }
                  go(receive(Some(toFeed)), if (suffix.isEmpty) tl else tl.cons(suffix))
              }
          }
      }
    }
    in => go(Pipe.stepper(p), in).stream
  }

  def liftR[A, B, C](p: Pipe[Pure, A, B]): Pipe[Pure, Either[C, A], Either[C, B]] = {
    def swap[X, Y]: Pipe[Pure, Either[X, Y], Either[Y, X]] = _.map(_.swap)
    swap[C, A].andThen(liftL(p)).andThen(swap[B, C])
  }

  implicit class StepperOps[A, B](val self: Stepper[A, B]) extends AnyVal {
    def stepToAwait[I](
      cont: (Segment[B,Unit], Option[Segment[A,Unit]] => Stepper[A, B]) => Pull[Pure, I, Unit]
    ): Pull[Pure, I, Unit] = outer.stepToAwait(self)(cont)
  }

  def stepToAwait[A, B, I](s: Stepper[A, B], acc: Segment[B,Unit] = Segment.empty)(
    cont: (Segment[B,Unit], Option[Segment[A,Unit]] => Stepper[A, B]) => Pull[Pure, I, Unit]
  ): Pull[Pure, I, Unit] = {
    s.step match {
      case Stepper.Done => Pull.done
      case Stepper.Fail(err) => Pull.fail(err)
      case Stepper.Emits(segment, next) =>
        stepToAwait(next, acc ++ segment)(cont)
      case Stepper.Await(receive) =>
        cont(acc, receive)
    }
  }
}
