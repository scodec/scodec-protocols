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
package mpeg
package transport
package psi

import fs2._

sealed abstract class TransportStreamIndex {
  import TransportStreamIndex._

  def pat: Option[ProgramAssociationTable]
  def cat: Option[ConditionalAccessTable]

  def pmt(prg: ProgramNumber): Either[LookupError, ProgramMapTable]

  def programMapRecords(program: ProgramNumber, streamType: StreamType): Either[LookupError, List[ProgramMapRecord]] =
    for {
      p <- pat.toRight(LookupError.MissingProgramAssociation)
      _ <- p.programByPid.get(program).toRight(LookupError.UnknownProgram)
      q <- pmt(program)
      pmrs <- q.componentStreamMapping.get(streamType).toRight(LookupError.UnknownStreamType)
    } yield pmrs

  def programManRecord(program: ProgramNumber, streamType: StreamType): Either[LookupError, ProgramMapRecord] =
    programMapRecords(program, streamType).map { _.head }

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
      copy(pat = Some(pat), pmts = pmts.view.filterKeys(programs).toMap)
    }

    def withPmt(pmt: ProgramMapTable): TransportStreamIndex = {
      copy(pmts = pmts + (pmt.programNumber -> pmt))
    }

    def withCat(cat: ConditionalAccessTable): TransportStreamIndex =
      copy(cat = Some(cat))
  }

  def empty: TransportStreamIndex = DefaultTransportStreamIndex(None, None, Map.empty)

  def build: Transform.Aux[TransportStreamIndex, Table, Either[TransportStreamIndex, Table]] = Transform.stateful(empty) { (tsi, section) =>
    val updatedTsi = section match {
      case pat: ProgramAssociationTable =>
        Some(tsi.withPat(pat))
      case pmt: ProgramMapTable =>
        Some(tsi.withPmt(pmt))
      case cat: ConditionalAccessTable =>
        Some(tsi.withCat(cat))
      case other => None
    }
    val out = updatedTsi match {
      case Some(newTsi) if newTsi != tsi =>
        Chunk(Right(section), Left(newTsi))
      case _ =>
        Chunk(Right(section))
    }
    (updatedTsi.getOrElse(tsi), out)
  }
}
