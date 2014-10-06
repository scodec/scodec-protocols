package scodec.protocols
package mpeg
package transport

import scalaz.{ \/, \/-, -\/, NonEmptyList }
import scalaz.syntax.nel._
import \/.{ left, right }

import scalaz.stream.{ Cause, Process, Process1, process1 }

package object psi {

  def desectionExtendedSections[A <: ExtendedSection]: Process1[A, DesectioningError \/ NonEmptyList[A]] = {
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

  def desection: Process1[Section, DesectioningError \/ GroupedSections] = {
    // Lift output NEL type to a supertype
    val des: Process1[ExtendedSection, DesectioningError \/ GroupedSections] = desectionExtendedSections.map { _ map { sections =>
      GroupedSections(sections.head.tableId, sections)
    }}
    process1ext.conditionallyFeed(des, {
      case s: ExtendedSection => left(s)
      case s: Section => right(right(GroupedSections(s.tableId, s.wrapNel)))
    })
  }
}
