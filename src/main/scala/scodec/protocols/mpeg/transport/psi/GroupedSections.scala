package scodec.protocols
package mpeg
package transport
package psi

import language.higherKinds

import scala.reflect.ClassTag

import fs2._
import fs2.pipe.Stepper

import pipes._

/**
 * Group of sections that make up a logical message.
 *
 * Intermediate representation between sections and tables. All sections must share the same table id.
 */
sealed abstract class GroupedSections[+A <: Section] {
  def tableId: Int

  def head: A
  def tail: List[A]

  def list: List[A]
}

object GroupedSections {
  implicit class InvariantOps[A <: Section](val self: GroupedSections[A]) extends AnyVal {
    def narrow[B <: A : ClassTag]: Option[GroupedSections[B]] = {
      val matched = self.list.foldLeft(true) { (acc, s) => s match { case _: B => true; case _ => false } }
      if (matched) Some(self.asInstanceOf[GroupedSections[B]])
      else None
    }
  }

  private case class DefaultGroupedSections[A <: Section](head: A, tail: List[A]) extends GroupedSections[A] {
    val tableId = head.tableId
    val list = head :: tail
  }

  def apply[A <: Section](head: A, tail: List[A] = Nil): GroupedSections[A] =
    DefaultGroupedSections[A](head, tail)

  def groupExtendedSections[F[_], A <: ExtendedSection]: Pipe[F, A, Either[GroupingError, GroupedSections[A]]] = {
    type Key = (Int, Int)
    def toKey(section: A): Key = (section.tableId, section.extension.tableIdExtension)

    def go(accumulatorByIds: Map[Key, SectionAccumulator[A]]): Handle[F, A] => Pull[F, Either[GroupingError, GroupedSections[A]], Handle[F, A]] = h => {
      h.receive1 { (section, tl) =>
        val key = toKey(section)
        val (err, acc) = accumulatorByIds.get(key) match {
          case None => (None, SectionAccumulator(section))
          case Some(acc) =>
            acc.add(section) match {
              case Right(acc) => (None, acc)
              case Left(err) =>
                (Some(err), SectionAccumulator(section))
            }
        }
        val maybeOutputErr = err.map { e => Pull.output1(Left(GroupingError(section.tableId, section.extension.tableIdExtension, e))) }.getOrElse(Pull.pure(()))
        maybeOutputErr >> (acc.complete match {
          case None => go(accumulatorByIds + (key -> acc))(tl)
          case Some(sections) =>
            Pull.output1(Right(sections)) >> go(accumulatorByIds - key)(tl)
        })
      }
    }

    _ pull go(Map.empty)
  }

  def noGrouping[F[_]]: Pipe[F, Section, Either[GroupingError, GroupedSections[Section]]] =
    pipe.lift(s => Right(GroupedSections(s)))

  /**
   * Groups sections in to groups.
   *
   * Extended sections, aka sections with the section syntax indicator set to true, are automatically handled.
   * Non-extended sections are emitted as singleton groups.
   */
  def group[F[_]]: Pipe[F, Section, Either[GroupingError, GroupedSections[Section]]] = {
    groupGeneral(noGrouping)
  }

  /**
   * Groups sections in to groups.
   *
   * Extended sections, aka sections with the section syntax indicator set to true, are automatically handled.
   * The specified `nonExtended` process is used to handle non-extended sections.
   */
  def groupGeneral(nonExtended: Pipe[Pure, Section, Either[GroupingError, GroupedSections[Section]]]): Pipe[Pure, Section, Either[GroupingError, GroupedSections[Section]]] = {
    groupGeneralConditionally(nonExtended, _ => true)
  }

  /**
   * Groups sections in to groups.
   *
   * Extended sections, aka sections with the section syntax indicator set to true, are automatically handled if `true` is returned from the
   * `groupExtended` function when applied with the section in question.
   *
   * The specified `nonExtended` transducer is used to handle non-extended sections.
   */
  def groupGeneralConditionally(nonExtended: Pipe[Pure, Section, Either[GroupingError, GroupedSections[Section]]], groupExtended: ExtendedSection => Boolean = _ => true): Pipe[Pure, Section, Either[GroupingError, GroupedSections[Section]]] = {

    type ThisPull = Pull[Pure, Either[GroupingError, GroupedSections[Section]], Handle[Pure, Section]]

    def go(
      ext: Option[Chunk[ExtendedSection]] => Stepper[ExtendedSection, Either[GroupingError, GroupedSections[ExtendedSection]]],
      nonExt: Option[Chunk[Section]] => Stepper[Section, Either[GroupingError, GroupedSections[Section]]]
    ): Handle[Pure, Section] => ThisPull = h => {
      h.receive1 { (section, tl) =>
        section match {
          case s: ExtendedSection if groupExtended(s) =>
            ext(Some(Chunk.singleton(s))).stepToAwait { (out, next) =>
              Pull.output(Chunk.indexedSeq(out)) >> go(next, nonExt)(tl)
            }
          case s: Section =>
            nonExt(Some(Chunk.singleton(s))).stepToAwait { (out, next) =>
              Pull.output(Chunk.indexedSeq(out)) >> go(ext, next)(tl)
            }
        }
      }
    }

    _ pull { h =>
      pipe.stepper(groupExtendedSections[Pure, ExtendedSection]).stepToAwait { (out1, ext) =>
        pipe.stepper(nonExtended).stepToAwait { (out2, nonExt) =>
          Pull.output(Chunk.indexedSeq(out1 ++ out2)) >> go(ext, nonExt)(h)
        }
      }
    }
  }
}
