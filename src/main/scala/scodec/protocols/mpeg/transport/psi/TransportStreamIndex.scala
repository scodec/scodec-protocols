package scodec.protocols
package mpeg
package transport
package psi

import language.higherKinds

import fs2._

sealed abstract class TransportStreamIndex {
  import TransportStreamIndex._

  def pat: Option[ProgramAssociationTable]
  def cat: Option[ConditionalAccessTable]

  def pmt(prg: ProgramNumber): Either[LookupError, ProgramMapTable]

  def programMapRecords(program: ProgramNumber, streamType: StreamType): Either[LookupError, List[ProgramMapRecord]] =
    for {
      p <- pat.toRight(LookupError.MissingProgramAssociation).right
      _ <- p.programByPid.get(program).toRight(LookupError.UnknownProgram).right
      q <- pmt(program).right
      pmrs <- q.componentStreamMapping.get(streamType).toRight(LookupError.UnknownStreamType).right
    } yield pmrs

  def programManRecord(program: ProgramNumber, streamType: StreamType): Either[LookupError, ProgramMapRecord] =
    programMapRecords(program, streamType).right.map { _.head }

  def withPat(pat: ProgramAssociationTable): TransportStreamIndex
  def withPmt(pmt: ProgramMapTable): TransportStreamIndex
  def withCat(cat: ConditionalAccessTable): TransportStreamIndex
}


object TransportStreamIndex {

  sealed abstract class LookupError
  object LookupError {
    case object UnknownProgram extends LookupError
    case object UnknownStreamType extends LookupError
    case object MissingProgramAssociation extends LookupError
    case object MissingProgramMap extends LookupError
  }

  private case class DefaultTransportStreamIndex(
    pat: Option[ProgramAssociationTable],
    cat: Option[ConditionalAccessTable],
    pmts: Map[ProgramNumber, ProgramMapTable]
  ) extends TransportStreamIndex {

    def pmt(prg: ProgramNumber): Either[LookupError, ProgramMapTable] =
      pmts.get(prg).toRight(LookupError.UnknownProgram)

    def withPat(pat: ProgramAssociationTable): TransportStreamIndex = {
      val programs = pat.programByPid.keys.toSet
      copy(pat = Some(pat), pmts = pmts filterKeys programs)
    }

    def withPmt(pmt: ProgramMapTable): TransportStreamIndex = {
      copy(pmts = pmts + (pmt.programNumber -> pmt))
    }

    def withCat(cat: ConditionalAccessTable): TransportStreamIndex =
      copy(cat = Some(cat))
  }

  def empty: TransportStreamIndex = DefaultTransportStreamIndex(None, None, Map.empty)

  def build[F[_]]: Pipe[F, Table, Either[TransportStreamIndex, Table]] = {
    def go(tsi: TransportStreamIndex, s: Stream[F, Table]): Pull[F, Either[TransportStreamIndex, Table], Unit] = {
      s.pull.uncons1.flatMap {
        case Some((section, tl)) =>
          val updatedTsi = section match {
            case pat: ProgramAssociationTable =>
              Some(tsi.withPat(pat))
            case pmt: ProgramMapTable =>
              Some(tsi.withPmt(pmt))
            case cat: ConditionalAccessTable =>
              Some(tsi.withCat(cat))
            case other => None
          }
          updatedTsi match {
            case Some(newTsi) if newTsi != tsi =>
              Pull.output1(Right(section)) >> Pull.output1(Left(newTsi)) >> go(newTsi, tl)
            case _ =>
              Pull.output1(Right(section)) >> go(tsi, tl)
          }
        case None => Pull.done
      }
    }
    in => go(TransportStreamIndex.empty, in).stream
  }
}
