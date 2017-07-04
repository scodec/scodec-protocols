package scodec.protocols

import language.higherKinds

import fs2._

final class Transform[S,-I,+O] private (val initial: S, val transform: (S,I) => Segment[O,S]) {

  def toPipe[F[_]]: Pipe[F,I,O] =
    _.scanSegments(initial)((state, segment) => segment.flatMapAccumulate(state)(transform).mapResult(_._2))

  def andThen[S2,O2](t: Transform[S2,O,O2]): Transform[(S,S2),I,O2] =
    new Transform[(S,S2),I,O2]((initial, t.initial), { case ((s,s2),i) =>
      transform(s,i).flatMapAccumulate(s2)(t.transform)
    })

  def map[O2](f: O => O2): Transform[S,I,O2] =
    new Transform[S,I,O2](initial, (s,i) => transform(s, i).map(f))

  def contramap[I2](f: I2 => I): Transform[S,I2,O] =
    new Transform[S,I2,O](initial, (s,i2) => transform(s, f(i2)))

  def xmapState[S2](g: S => S2)(f: S2 => S): Transform[S2,I,O] =
    new Transform[S2,I,O](g(initial), (s2,i) => transform(f(s2),i).mapResult(g))

  def lens[I2,O2](get: I2 => I, set: (I2, O) => O2): Transform[S,I2,O2] =
    new Transform[S,I2,O2](initial, (s,i2) => transform(s, get(i2)).map(s => set(i2, s)))

  def first[A]: Transform[S, (I, A), (O, A)] =
    lens(_._1, (t, o) => (o, t._2))

  def second[A]: Transform[S, (A, I), (A, O)] =
    lens(_._2, (t, o) => (t._1, o))

  def semilens[I2,O2](extract: I2 => Either[O2, I], inject: (I2, O) => O2): Transform[S,I2,O2] =
    new Transform[S,I2,O2](initial, (s,i2) => extract(i2).fold(
      o2 => Chunk.singleton(o2).mapResult(_ => s),
      i => transform(s, i).map(s => inject(i2, s))))

  def semipass[I2,O2 >: O](extract: I2 => Either[O2, I]): Transform[S,I2,O2] = semilens(extract, (_, o) => o)

  def left[A]: Transform[S, Either[I, A], Either[O, A]] =
    semilens(_.fold(i => Right(i), a => Left(Right(a))), (_, o) => Left(o))

  def right[A]: Transform[S, Either[A, I], Either[A, O]] =
    semilens(_.fold(a => Left(Left(a)), i => Right(i)), (_, o) => Right(o))

  def choice[S2,I2,O2 >: O](t: Transform[S2,I2,O2]): Transform[(S,S2),Either[I,I2],O2] = new Transform[(S,S2),Either[I,I2],O2]((initial, t.initial), { case ((s,s2),e) =>
    e match {
      case Left(i) => transform(s,i).mapResult(_ -> s2)
      case Right(i2) => t.transform(s2,i2).mapResult(s -> _)
    }
  })

  def either[S2,I2,O2](t: Transform[S2,I2,O2]): Transform[(S,S2),Either[I,I2],Either[O,O2]] = new Transform[(S,S2),Either[I,I2],Either[O,O2]]((initial, t.initial), { case ((s,s2),e) =>
    e match {
      case Left(i) => transform(s,i).map(Left(_)).mapResult(_ -> s2)
      case Right(i2) => t.transform(s2,i2).map(Right(_)).mapResult(s -> _)
    }
  })
}

object Transform {
  def stateful[S,I,O](initial: S)(f: (S, I) => Segment[O,S]): Transform[S,I,O] =
    new Transform[S,I,O](initial, f)

  def stateful1[S,I,O](initial: S)(f: (S, I) => (S, O)): Transform[S,I,O] =
    stateful[S,I,O](initial) { (s,i) => val (s2,o) = f(s,i); Segment.singleton(o).asResult(s2) }

  def stateless[I,O](f: I => Segment[O,Unit]): Transform[Unit,I,O] =
    stateful[Unit,I,O](())((u,i) => f(i))

  def lift[I,O](f: I => O): Transform[Unit,I,O] =
    stateless(i => Chunk.singleton(f(i)))
}
