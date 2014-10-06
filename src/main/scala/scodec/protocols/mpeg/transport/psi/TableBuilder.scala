package scodec.protocols
package mpeg
package transport
package psi

import scalaz.{ \/, \/-, -\/, NonEmptyList }
import \/.{ left, right }
import scalaz.stream._

case class TableBuildingError(tableId: Int, message: String) extends MpegError

class TableBuilder private (cases: Map[Int, TableSupport[_]]) {

  def supporting[T <: Table : TableSupport]: TableBuilder = {
    val ts = implicitly[TableSupport[T]]
    new TableBuilder(cases + (ts.tableId -> ts))
  }

  def sectionsToTables: Process1[GroupedSections, TableBuildingError \/ Table] = {
    Process.await1[GroupedSections].flatMap { gs =>
      cases.get(gs.tableId) match {
        case None => Process.halt
        case Some(ts) =>
          ts.toTable(gs) match {
            case \/-(table: Table) => Process.emit(right(table))
            case -\/(err) => Process.emit(left(TableBuildingError(gs.tableId, err)))
          }
      }
    }.repeat
  }
}

object TableBuilder {

  def empty: TableBuilder = new TableBuilder(Map.empty)

  def supporting[T <: Table : TableSupport] = empty.supporting[T]

  def psi: TableBuilder = supporting[ProgramAssociationTable].supporting[ProgramMapTable].supporting[ConditionalAccessTable]
}

trait TableSupport[T <: Table] {
  def tableId: Int
  def toTable(gs: GroupedSections): String \/ T
  def toSections(t: T): NonEmptyList[Section]
}
