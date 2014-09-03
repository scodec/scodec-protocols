package scodec.protocols

import scalaz.{ Lens, LensFamily }
import scalaz.stream.{ Process, Process1, process1 }
import Process._
import process1.Await1

/** Integrates lenses and processes. */
object LensCombinators {

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
    def go(last: Option[A1], cur: Process1[B1, B2]): Process1[A1, A2] = cur.step match {
      case h@Halt(_) => h
      case Step(Emit(h), cont) =>
        val step = last match {
          case Some(a) => Process.emitAll(h map { b => l.set(a, b) })
          case None => Process.halt
        }
        step ++ go(last, cont.continue)
      case Step(Await1(rcv), cont) =>
        await1[A1].flatMap { a => go(Some(a), process1.feed1(l.get(a))(cont.continue)) }
    }
    go(None, p)
  }
}
