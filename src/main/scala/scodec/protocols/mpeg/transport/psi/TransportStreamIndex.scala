package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/, NonEmptyList }
import \/.{ right, left }
import scalaz.syntax.std.option._
import scalaz.stream._

sealed abstract class TransportStreamIndex {
  import TransportStreamIndex._

  def pat: Option[ProgramAssociationTable]
  def cat: Option[ConditionalAccessTable]

  def pmt(prg: ProgramNumber): LookupError \/ ProgramMapTable

  def programMapRecords(program: ProgramNumber, streamType: StreamType): LookupError \/ NonEmptyList[ProgramMapRecord] =
    for {
      p <- pat \/> LookupError.MissingProgramAssociation
      _ <- p.programByPid.get(program) \/> LookupError.UnknownProgram
      q <- pmt(program)
      pmrs <- q.componentStreamMapping.get(streamType) \/> LookupError.UnknownStreamType
    } yield pmrs

  def programManRecord(program: ProgramNumber, streamType: StreamType): LookupError \/ ProgramMapRecord =
    programMapRecords(program, streamType) map { _.head }

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

    def pmt(prg: ProgramNumber): LookupError \/ ProgramMapTable =
      pmts.get(prg) \/> LookupError.UnknownProgram

    def withPat(pat: ProgramAssociationTable): TransportStreamIndex = {
      val programs = pat.programByPid.keys.toSet
      copy(pat = Some(pat), pmts = pmts filterKeys programs)
    }

    def withPmt(pmt: ProgramMapTable): TransportStreamIndex = {
      val add = pat.cata(_.programByPid.contains(pmt.programNumber), true)
      if (add) copy(pmts = pmts + (pmt.programNumber -> pmt))
      else this
    }

    def withCat(cat: ConditionalAccessTable): TransportStreamIndex =
      copy(cat = Some(cat))
  }

  def empty: TransportStreamIndex = DefaultTransportStreamIndex(None, None, Map.empty)

  def build: Writer1[TransportStreamIndex, Table, Table] = {
    def go(tsi: TransportStreamIndex): Writer1[TransportStreamIndex, Table, Table] = {
      def recurse(a: Table, newTsi: TransportStreamIndex): Writer1[TransportStreamIndex, Table, Table] =
        Process.emit(right(a)) ++ (if (newTsi == tsi) Process.halt else Process.emit(left(newTsi))) ++ go(newTsi)

      Process.await1[Table].flatMap {
        case pat: ProgramAssociationTable =>
          recurse(pat, tsi.withPat(pat))
        case pmt: ProgramMapTable =>
          recurse(pmt, tsi.withPmt(pmt))
        case cat: ConditionalAccessTable =>
          recurse(cat, tsi.withCat(cat))
        case other => Process.emit(right(other)) ++ go(tsi)
      }
    }
    go(TransportStreamIndex.empty)
  }
}
