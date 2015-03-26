package scodec.protocols

import language.higherKinds

import scalaz.{ \/, -\/, \/-, Bind, Lens, LensFamily, Traverse }
import \/.{ left, right }
import scalaz.syntax.traverse._
import scalaz.stream.{ Cause, Process, Process0, Process1, process1 }
import Process._
import process1.Await1

/** General purpose combinators for working with `Process1` that are not included in scalaz-stream. */
object process1ext {

  /**
   * Lifts a `Process1[B, B]` to a `Process1[A, A]` using the provided lens.
   *
   * Values fed to this process are converted to type `B` and fed to
   * `p`. Any `B` values emitted by `p` are re-emitted as `A` values by
   * setting each `B` in to the last emitted `A`. Hence, the last fed `A`
   * is kept in memory by this process.
   *
   * Note that this halts whenever `p` halts.
   */
  def lens[A, B](l: Lens[A, B])(p: Process1[B, B]): Process1[A, A] = lensf(l)(p)

  /**
   * Lifts a `Process1[B1, B2]` to a `Process1[A1, A2]` using the provided lens family.
   *
   * Values fed to this process are converted to type `B1` and fed to
   * `p`. Any `B2` values emitted by `p` are re-emitted as `A2` values by
   * setting each `B2` in to the last emitted `A1`. Hence, the last fed `A1`
   * is kept in memory by this process.
   *
   * Note that this halts whenever `p` halts.
   */
  def lensf[A1, A2, B1, B2](l: LensFamily[A1, A2, B1, B2])(p: Process1[B1, B2]): Process1[A1, A2] = {
    val paired: Process1[A1, (A1, B1)] = await1[A1].map { a1 => (a1, l.get(a1)) }.repeat
    val setB1: Process1[(A1, B1), A2] = liftSecond[B1, B2, A1](_ => None)(p).map { case (a1, b2) => l.set(a1, b2) }
    paired |> setB1
  }

  // see https://github.com/scalaz/scalaz-stream/pull/239
  private def liftFirst[A, B, C](f: B => Option[C])(p: Process1[A, B]): Process1[(A, C), (B, C)] = {
    def go(curr: Process1[A, B]): Process1[(A, C), (B, C)] = {
      val cleanup: Process1[(A, C), (B, C)] = curr.disconnect(Cause.Kill).flatMap(b => f(b) match {
        case Some(c) => emit((b, c))
        case None => halt
      })
      receive1Or[(A, C), (B, C)](cleanup) { case (a, c) =>
        val (emitted, next) = curr.feed1(a).unemit
        val out = emitAll(emitted).map((_, c))
        next match {
          case h @ Halt(_) => out fby h
          case other => out fby go(other)
        }
      }
    }
    go(p)
  }

  private def liftSecond[A, B, C](f: B => Option[C])(p: Process1[A, B]): Process1[(C, A), (C, B)] =
    process1.lift[(C, A), (A, C)](_.swap) |> liftFirst(f)(p).map(_.swap)

  /**
   * Accepts values of type `X` and converts them to an `A` or `B`. If `A`, the `A`
   * is fed to `p`. If `B`, the `B` is emitted directly.
   */
  def conditionallyFeed[A, B, X](p: Process1[A, B], f: X => A \/ B): Process1[X, B] =
    process1.lift(f).pipe(p.liftL).map(_.fold(identity, identity))

  def mapRight[I, L, A, B](p: Process1[I, L \/ A])(f: A => B): Process1[I, L \/ B] =
    p.map { _ map f }

  /** Stream transducer that drops left values from a stream of `F[A \/ B]` values. */
  def drainFL[F[_]: Traverse, A, B]: Process1[F[A \/ B], F[B]] =
    process1.id[F[A \/ B]] flatMap { fab =>
      fab.traverse(_.fold(_ => halt, emit))(Process.processMonadPlus[Nothing])
    }

  /** Stream transducer that drops right values from a stream of `F[A \/ B]` values. */
  def drainFR[F[_]: Traverse, A, B]: Process1[F[A \/ B], F[A]] =
    process1.lift((fab: F[A \/ B]) => fab.map { _.swap }) pipe drainFL
}
