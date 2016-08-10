package scodec.protocols
package mpeg
package transport
package psi

import language.higherKinds

import fs2._
import shapeless.Typeable

case class TableBuildingError(tableId: Int, message: String) extends MpegError

class TableBuilder private (cases: Map[Int, List[TableSupport[_]]]) {

  def supporting[T <: Table](implicit ts: TableSupport[T]): TableBuilder = {
    val newCases = ts :: cases.getOrElse(ts.tableId, Nil)
    new TableBuilder(cases + (ts.tableId -> newCases))
  }

  def sectionsToTables[F[_]]: Pipe[F, GroupedSections[Section], Either[TableBuildingError, Table]] = {
    def go: Handle[F, GroupedSections[Section]] => Pull[F, Either[TableBuildingError, Table], Handle[F, GroupedSections[Section]]] = h => {
      h.receive1 { (gs, tl) =>
        cases.get(gs.tableId) match {
          case None | Some(Nil) =>
            Pull.output1(Left(TableBuildingError(gs.tableId, "Unknown table id"))) >> go(tl)
          case Some(list) =>
            list.dropRight(1).foldRight[Either[String, _]](list.last.toTable(gs)) { (next, res) => res.fold(_ => next.toTable(gs), Right(_)) } match {
              case Right(table) => Pull.output1(Right(table.asInstanceOf[Table])) >> go(tl)
              case Left(err) => Pull.output1(Left(TableBuildingError(gs.tableId, err))) >> go(tl)
            }
          }
        }
      }

    _ pull go
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
  def toTable(gs: GroupedSections[Section]): Either[String, T]
  def toSections(t: T): GroupedSections[Section]
}

object TableSupport {

  def singleton[A <: Section with Table : reflect.ClassTag](tableId: Int)(implicit t: Typeable[A]): TableSupport[A] = {
    val tid = tableId
    new TableSupport[A] {
      def tableId = tid
      def toTable(gs: GroupedSections[Section]) =
        gs.narrow[A].toRight(s"Not a ${t.describe}").right.flatMap { sections =>
          if (sections.tail.isEmpty) Right(sections.head)
          else Left(s"${t.describe} supports only 1 section but got ${sections.list.size}")
        }
      def toSections(table: A) = GroupedSections(table)
    }
  }
}
