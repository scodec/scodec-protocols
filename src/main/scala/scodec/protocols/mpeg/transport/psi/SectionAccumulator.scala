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

package scodec.protocols.mpeg
package transport
package psi

/** Accumulates sections of the same table id and table id extension. */
private[psi] class SectionAccumulator[A <: ExtendedSection] private (val sections: GroupedSections[A], sectionByNumber: Map[Int, A]) {

  def add(section: A): Either[String, SectionAccumulator[A]] = {
    def validate(err: => String)(f: Boolean): Either[String, Unit] =
      if (f) Right(()) else Left(err)

    def checkEquality[B](name: String)(f: A => B): Either[String, Unit] =
      validate(name + " do not match")(f(section) == f(sections.head))

    val sectionNumber = section.extension.sectionNumber
    for {
      _ <- checkEquality("table ids")(_.tableId)
      _ <- checkEquality("table id extensions")(_.extension.tableIdExtension)
      _ <- checkEquality("versions")(_.extension.version)
      _ <- checkEquality("last section numbers")(_.extension.lastSectionNumber)
      _ <- validate("invalid section number")(sectionNumber <= sections.head.extension.lastSectionNumber)
      _ <- validate("duplicate section number")(!sectionByNumber.contains(sectionNumber))
    } yield new SectionAccumulator(GroupedSections(section, sections.list), sectionByNumber + (section.extension.sectionNumber -> section))
  }

  def complete: Option[GroupedSections[A]] =
    if (sectionByNumber.size == (sections.head.extension.lastSectionNumber + 1)) Some(sections) else None
}

private[psi] object SectionAccumulator {

  def apply[A <: ExtendedSection](section: A): SectionAccumulator[A] =
    new SectionAccumulator(GroupedSections(section), Map(section.extension.sectionNumber -> section))
}


