package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/, NonEmptyList }
import \/.{ left, right }
import scalaz.stream._
import scalaz.syntax.std.option._

import shapeless.Typeable

case class TableBuildingError(tableId: Int, message: String) extends MpegError

class TableBuilder private (cases: Map[Int, List[TableSupport[_]]]) {

  def supporting[T <: Table : TableSupport]: TableBuilder = {
    val ts = implicitly[TableSupport[T]]
    val newCases = ts :: cases.getOrElse(ts.tableId, Nil)
    new TableBuilder(cases + (ts.tableId -> newCases))
  }

  def sectionsToTables: Process1[GroupedSections, TableBuildingError \/ Table] = {
    Process.await1[GroupedSections].flatMap { gs =>
      cases.get(gs.tableId) match {
        case None | Some(Nil) => Process.halt
        case Some(list) =>
          list.dropRight(1).foldRight[String \/ _](list.last.toTable(gs)) { (next, res) => res orElse next.toTable(gs) } match {
            case \/-(table) => Process.emit(right(table.asInstanceOf[Table]))
            case -\/(err) => Process.emit(left(TableBuildingError(gs.tableId, err)))
          }
      }
    }.repeat
  }
}

object TableBuilder {

  def empty: TableBuilder = new TableBuilder(Map.empty)

  def supporting[T <: Table : TableSupport] = empty.supporting[T]

  def psi: TableBuilder =
    supporting[ProgramAssociationTable].
    supporting[ProgramMapTable].
    supporting[ConditionalAccessTable]
}

trait TableSupport[T <: Table] {
  def tableId: Int
  def toTable(gs: GroupedSections): String \/ T
  def toSections(t: T): NonEmptyList[Section]
}

object TableSupport {

  def singleton[A <: Section with Table : reflect.ClassTag](tableId: Int)(implicit t: Typeable[A]): TableSupport[A] = {
    val tid = tableId
    new TableSupport[A] {
      def tableId = tid
      def toTable(gs: GroupedSections) =
        gs.as[A].toRightDisjunction(s"Not a ${t.describe}").flatMap { sections =>
          if (sections.tail.isEmpty) \/.right(sections.head)
          else \/.left(s"${t.describe} supports only 1 section but got ${sections.list.size}")
        }
      def toSections(table: A) = NonEmptyList(table)
    }
  }
}
