package scodec.protocols
package mpeg
package transport
package psi

import scala.reflect.ClassTag
import scalaz.{ NonEmptyList, Tag, Tags }
import scalaz.std.AllInstances._
import scalaz.syntax.foldable1._

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
}
