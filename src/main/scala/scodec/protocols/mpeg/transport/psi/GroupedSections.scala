package scodec.protocols
package mpeg
package transport
package psi

import scala.reflect.ClassTag
import scalaz.{ \/, \/-, -\/, NonEmptyList, Tag, Tags }
import \/.{ left, right }
import scalaz.std.AllInstances._
import scalaz.syntax.foldable1._
import scalaz.syntax.nel._
import scalaz.stream.{ Cause, Process, Process1, process1 }

/** Group of sections that make up a logical message. Intermediate representation between sections and tables. */
sealed abstract class GroupedSections {
  def tableId: Int
  def nel: NonEmptyList[Section]
  def as[A <: Section : ClassTag]: Option[NonEmptyList[A]]
}

object GroupedSections {
  private case class DefaultGroupedSections(tableId: Int, nel: NonEmptyList[Section]) extends GroupedSections {
    def as[A: ClassTag]: Option[NonEmptyList[A]] = {
      if (Tag.unwrap(nel.foldMap1(x => Tags.Conjunction(x.isInstanceOf[A]))))
        Some(nel.asInstanceOf[NonEmptyList[A]])
      else None
    }
  }

  def apply(tableId: Int, sections: NonEmptyList[Section]): GroupedSections =
    DefaultGroupedSections(tableId, sections)

  def groupExtendedSections[A <: ExtendedSection]: Process1[A, GroupingError \/ NonEmptyList[A]] = {
    type Key = (Int, Int)
    def toKey(section: A): Key = (section.tableId, section.extension.tableIdExtension)

    def go(accumulatorByIds: Map[Key, SectionAccumulator[A]]): Process1[A, GroupingError \/ NonEmptyList[A]] = {
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
        val errStream = err.map { e => Process.emit(left(GroupingError(section.tableId, section.extension.tableIdExtension, e))) }.getOrElse(Process.halt)
        errStream ++ (acc.complete match {
          case None => go(accumulatorByIds + (key -> acc))
          case Some(sections) =>
            Process.emit(right(sections)) ++ go(accumulatorByIds - key)
        })
      }
    }

    go(Map.empty)
  }

  /**
   * Groups sections in to groups.
   *
   * Extended sections, aka sections with the section syntax indicator set to true, are automatically handled.
   * Non-extended sections are emitted as singleton groups.
   */
  def group: Process1[Section, GroupingError \/ GroupedSections] = {
    groupGeneral(process1.lift(s => right(GroupedSections(s.tableId, s.wrapNel))))
  }

  /**
   * Groups sections in to groups.
   *
   * Extended sections, aka sections with the section syntax indicator set to true, are automatically handled.
   * The specified `nonExtended` process is used to handle non-extended sections.
   */
  def groupGeneral(nonExtended: Process1[Section, GroupingError \/ GroupedSections]): Process1[Section, GroupingError \/ GroupedSections] = {
    def go(
      ext: Process1[ExtendedSection, GroupingError \/ GroupedSections],
      nonExt: Process1[Section, GroupingError \/ GroupedSections]
    ): Process1[Section, GroupingError \/ GroupedSections] = {
      Process.receive1Or[Section, GroupingError \/ GroupedSections](ext.disconnect(Cause.Kill) ++ nonExt.disconnect(Cause.Kill)) {
        case s: ExtendedSection =>
          val (out, next) = process1.feed1(s)(ext).unemit
          Process.emitAll(out) ++ go(next, nonExt)
        case s: Section =>
          val (out, next) = process1.feed1(s)(nonExt).unemit
          Process.emitAll(out) ++ go(ext, next)
      }
    }
    val des: Process1[ExtendedSection, GroupingError \/ GroupedSections] = groupExtendedSections.map { _ map { sections =>
      GroupedSections(sections.head.tableId, sections)
    }}
    go(des, nonExtended)
  }
}
