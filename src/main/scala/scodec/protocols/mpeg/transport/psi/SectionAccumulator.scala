package scodec.protocols.mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/, NonEmptyList }
import \/.{ left, right }
import scalaz.syntax.nel._
import scalaz.syntax.std.boolean._

import scalaz.stream.{ Process, Process1 }

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

object SectionAccumulator {

  def apply[A <: ExtendedSection](section: A): SectionAccumulator[A] =
    new SectionAccumulator(section.wrapNel, Map(section.extension.sectionNumber -> section))

  def desection[A <: ExtendedSection]: Process1[A, DesectioningError \/ NonEmptyList[A]] = {
    type Key = (Int, Int)
    def toKey(section: A): Key = (section.tableId, section.extension.tableIdExtension)

    def go(accumulatorByIds: Map[Key, SectionAccumulator[A]]): Process1[A, DesectioningError \/ NonEmptyList[A]] = {
      Process.await1[A].flatMap { section =>
        val key = toKey(section)
        val (err, acc) = accumulatorByIds.get(key) match {
          case None => (None, SectionAccumulator(section))
          case Some(acc) =>
            acc.add(section) match {
              case \/-(acc) => (None, acc)
              case -\/(err) =>
                (Some(err), SectionAccumulator(section))
            }
        }
        val errStream = err.map { e => Process.emit(left(DesectioningError(section.tableId, section.extension.tableIdExtension, e))) }.getOrElse(Process.halt)
        errStream ++ (acc.complete match {
          case None => go(accumulatorByIds + (key -> acc))
          case Some(sections) =>
            Process.emit(right(sections)) ++ go(accumulatorByIds - key)
        })
      }
    }

    go(Map.empty)
  }
}


