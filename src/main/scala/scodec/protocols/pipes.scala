package scodec.protocols

import fs2._
import fs2.pipe.Stepper

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
    pipe.lift(f).andThen(liftL(p)).andThen(pipe.lift(_.fold(identity, identity)))

  def liftL[A, B, C](p: Pipe[Pure, A, B]): Pipe[Pure, Either[A, C], Either[B, C]] = {
    def go(stepper: Stepper[A, B]): Stream.Handle[Pure, Either[A, C]] => Pull[Pure, Either[B, C], Stream.Handle[Pure, Either[A, C]]] = h => {
      stepper.step match {
        case Stepper.Done => Pull.done
        case Stepper.Fail(err) => Pull.fail(err)
        case Stepper.Emits(chunk, next) =>
          Pull.output(chunk.map { b => Left(b): Either[B, C] }) >> go(next)(h)
        case Stepper.Await(receive) =>
          h.receive {
            case chunk #: tl =>
              chunk.uncons match {
                case None =>
                  go(stepper)(tl)
                case Some((head @ Right(c), tail)) =>
                  val numHeadRights = {
                    val indexOfFirstLeft = tail.indexWhere(_.isLeft)
                    indexOfFirstLeft match {
                      case None => chunk.size
                      case Some(idx) => 1 + idx
                    }
                  }
                  val toOutput = chunk.take(numHeadRights).asInstanceOf[Chunk[Either[B, C]]]
                  val remainder = chunk.drop(numHeadRights)
                  Pull.output(toOutput) >> go(stepper)(if (remainder.isEmpty) tl else tl.push(remainder))
                case Some((Left(a), tail)) =>
                  val numHeadLefts = {
                    val indexOfFirstRight = tail.indexWhere(_.isRight)
                    indexOfFirstRight match {
                      case None => chunk.size
                      case Some(idx) => 1 + idx
                    }
                  }
                  val toFeed = chunk.take(numHeadLefts).map { case Left(a) => a; case Right(_) => sys.error("Chunk is all lefts!") }
                  val remainder = chunk.drop(numHeadLefts)
                  go(receive(Some(toFeed)))(if (remainder.isEmpty) tl else tl.push(remainder))
              }
          }
      }
    }
    _ pull go(pipe.stepper(p))
  }

  def liftR[A, B, C](p: Pipe[Pure, A, B]): Pipe[Pure, Either[C, A], Either[C, B]] = {
    def swap[X, Y]: Pipe[Pure, Either[X, Y], Either[Y, X]] = pipe.lift((_: Either[X, Y]).swap)
    swap[C, A].andThen(liftL(p)).andThen(swap[B, C])
  }

  implicit class StepperOps[A, B](val self: Stepper[A, B]) extends AnyVal {
    def stepToAwait[I, R](
      cont: (Vector[B], Option[Chunk[A]] => Stepper[A, B]) => Pull[Pure, I, R]
    ): Pull[Pure, I, R] = outer.stepToAwait(self)(cont)
  }

  def stepToAwait[A, B, I, R](s: Stepper[A, B], acc: Vector[B] = Vector.empty)(
    cont: (Vector[B], Option[Chunk[A]] => Stepper[A, B]) => Pull[Pure, I, R]
  ): Pull[Pure, I, R] = {
    s.step match {
      case Stepper.Done => Pull.done
      case Stepper.Fail(err) => Pull.fail(err)
      case Stepper.Emits(chunk, next) =>
        stepToAwait(next, acc ++ chunk.toVector)(cont)
      case Stepper.Await(receive) =>
        cont(acc, receive)
    }
  }
}
