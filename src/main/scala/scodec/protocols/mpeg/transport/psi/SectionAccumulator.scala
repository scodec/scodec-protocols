package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, NonEmptyList }
import \/.{ left, right }
import scalaz.syntax.nel._
import scalaz.syntax.std.boolean._

/** Accumulates sections of the same table id and table id extension. */
private[psi] class SectionAccumulator[A <: ExtendedSection] private (val sections: NonEmptyList[A], sectionByNumber: Map[Int, A]) {

  def add(section: A): String \/ SectionAccumulator[A] = {
    def validate(err: => String)(f: Boolean): String \/ Unit = {
      if (f) right(())
      else left(err)
    }

    def checkEquality[B](name: String)(f: A => B): String \/ Unit = {
      validate(name + " do not match")(f(section) == f(sections.head))
    }

    for {
      _ <- checkEquality("table ids")(_.tableId)
      _ <- checkEquality("table id extensions")(_.extension.tableIdExtension)
      _ <- checkEquality("versions")(_.extension.version)
      _ <- checkEquality("last section numbers")(_.extension.lastSectionNumber)
      sectionNumber = section.extension.sectionNumber
      _ <- validate("invalid section number")(sectionNumber <= sections.head.extension.lastSectionNumber)
      _ <- validate("duplicate section number")(!sectionByNumber.contains(sectionNumber))
    } yield new SectionAccumulator(section <:: sections, sectionByNumber + (section.extension.sectionNumber -> section))
  }

  def complete: Option[NonEmptyList[A]] =
    (sectionByNumber.size == (sections.head.extension.lastSectionNumber + 1)).option(sections)
}

private[psi] object SectionAccumulator {

  def apply[A <: ExtendedSection](section: A): SectionAccumulator[A] =
    new SectionAccumulator(section.wrapNel, Map(section.extension.sectionNumber -> section))
}


