/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.protocols

import language.higherKinds

import cats.data.Chain
import fs2._

sealed abstract class Transform[-I,+O] {

  type S
  val initial: S
  val transform: (S,I) => (S, Chunk[O])
  val onComplete: S => Chunk[O]

  def toPipe[F[_]]: Pipe[F,I,O] =
    _.pull.
      scanChunksOpt[S,O](initial) { state => 
        Some(chunk => Transform.flatMapAccumulate(chunk)(state)(transform))
      }.
      flatMap { state => Pull.output(onComplete(state)) }.
      stream


  def andThen[O2](t: Transform[O,O2]): Transform.Aux[(S,t.S),I,O2] =
    Transform[(S,t.S),I,O2]((initial, t.initial))({ case ((s,s2),i) =>
      val (sp, os) = transform(s, i)
      val (s2p, out) = Transform.flatMapAccumulate(os)(s2)(t.transform)
      ((sp, s2p), out)
    }, { case (s,s2) =>
      val (s3, out) = Transform.flatMapAccumulate(onComplete(s))(s2)(t.transform)
      Chunk.concat(List(out, t.onComplete(s3)))
    })

  def map[O2](f: O => O2): Transform.Aux[S,I,O2] =
    Transform[S,I,O2](initial)({ (s,i) => 
      val (s2, os) = transform(s, i)
      (s2, os.map(f))
    }, s => onComplete(s).map(f))

  def contramap[I2](f: I2 => I): Transform.Aux[S,I2,O] =
    Transform[S,I2,O](initial)((s,i2) => transform(s, f(i2)), onComplete)

  def xmapState[S2](g: S => S2)(f: S2 => S): Transform.Aux[S2,I,O] =
    Transform[S2,I,O](g(initial))({ (s2,i) => 
      val (s3, os) = transform(f(s2),i)
      (g(s3), os)
    }, s2 => onComplete(f(s2)))

  def lens[I2,O2](get: I2 => I, set: (I2, O) => O2): Transform.Aux[S,I2,O2] =
    Transform[S,I2,O2](initial)({ (s,i2) =>
      val (s2, os) = transform(s, get(i2))
      (s2, os.map(s => set(i2, s)))
    }, s => Chunk.empty)

  def first[A]: Transform.Aux[S,(I, A), (O, A)] =
    lens(_._1, (t, o) => (o, t._2))

  def second[A]: Transform.Aux[S,(A, I), (A, O)] =
    lens(_._2, (t, o) => (t._1, o))

  def semilens[I2,O2](extract: I2 => Either[O2, I], inject: (I2, O) => O2): Transform.Aux[S,I2,O2] =
    Transform[S,I2,O2](initial)(
      (s,i2) => extract(i2).fold(
        o2 => s -> Chunk.singleton(o2),
        i => {
          val (s2, os) = transform(s, i)
          (s2, os.map(o => inject(i2, o)))
        }),
      s => Chunk.empty
    )

  def semipass[I2,O2 >: O](extract: I2 => Either[O2, I]): Transform.Aux[S,I2,O2] = semilens(extract, (_, o) => o)

  def left[A]: Transform.Aux[S, Either[I, A], Either[O, A]] =
    semilens(_.fold(i => Right(i), a => Left(Right(a))), (_, o) => Left(o))

  def right[A]: Transform.Aux[S, Either[A, I], Either[A, O]] =
    semilens(_.fold(a => Left(Left(a)), i => Right(i)), (_, o) => Right(o))

  def choice[I2,O2 >: O](t: Transform[I2,O2]): Transform.Aux[(S,t.S),Either[I,I2],O2] =
    Transform[(S,t.S),Either[I,I2],O2]((initial, t.initial))({ case ((s,s2),e) =>
      e match {
        case Left(i) => 
          val (sp, os) = transform(s,i)
          ((sp, s2), os)
        case Right(i2) => 
          val (s2p, o2s) = t.transform(s2,i2)
          ((s, s2p), o2s)
      }
    }, { case (s,s2) => Chunk.concat(List(onComplete(s), t.onComplete(s2))) })

  def either[I2,O2](t: Transform[I2,O2]): Transform.Aux[(S,t.S),Either[I,I2],Either[O,O2]] =
    Transform[(S,t.S),Either[I,I2],Either[O,O2]]((initial, t.initial))({ case ((s,s2),e) =>
      e match {
        case Left(i) => 
          val (sp, os) = transform(s,i)
          ((sp, s2), os.map(Left(_)))
        case Right(i2) => 
          val (s2p, o2s) = t.transform(s2,i2)
          ((s, s2p), o2s.map(Right(_)))
      }
    }, { case (s,s2) => Chunk.concat(List(onComplete(s).map(Left(_)), t.onComplete(s2).map(Right(_)))) })
}

object Transform {
  type Aux[S0,-I,+O] = Transform[I,O] { type S = S0 }

  def apply[S0,I,O](initial0: S0)(transform0: (S0, I) => (S0, Chunk[O]), onComplete0: S0 => Chunk[O]): Transform.Aux[S0,I,O] =
    new Transform[I,O] {
      type S = S0
      val initial = initial0
      val transform = transform0
      val onComplete = onComplete0
    }

  def stateful[S,I,O](initial: S)(transform: (S, I) => (S, Chunk[O])): Transform.Aux[S,I,O] =
    apply(initial)(transform, _ => Chunk.empty)

  def stateful1[S,I,O](initial: S)(f: (S, I) => (S, O)): Transform.Aux[S,I,O] =
    stateful[S,I,O](initial) { (s,i) => val (s2,o) = f(s,i); s2 -> Chunk.singleton(o) }

  def stateless[I,O](f: I => Chunk[O]): Transform.Aux[Unit,I,O] =
    stateful[Unit,I,O](())((u,i) => (u, f(i)))

  def lift[I,O](f: I => O): Transform.Aux[Unit,I,O] =
    stateless(i => Chunk.singleton(f(i)))

  private def flatMapAccumulate[S, O, O2](c: Chunk[O])(s: S)(f: (S, O) => (S, Chunk[O2])): (S, Chunk[O2]) = {
    val (s2, acc) = c.foldLeft(s -> Chain.empty[Chunk[O2]]) { case ((s, acc), o) =>
      val (s2, o2s) = f(s, o)
      (s2, acc :+ o2s)
    }
    (s2, Chunk.concat(acc.toList))
  }
}
