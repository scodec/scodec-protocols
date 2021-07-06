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

package scodec.protocols
package mpeg
package transport
package psi

case class TableBuildingError(tableId: Int, message: String) extends MpegError

class TableBuilder private (cases: Map[Int, List[TableSupport[_]]]) {

  def supporting[T <: Table](implicit ts: TableSupport[T]): TableBuilder = {
    val newCases = ts :: cases.getOrElse(ts.tableId, Nil)
    new TableBuilder(cases + (ts.tableId -> newCases))
  }

  def build(gs: GroupedSections[Section]): Either[TableBuildingError, Table] = {
    cases.get(gs.tableId) match {
      case None | Some(Nil) => Left(TableBuildingError(gs.tableId, "Unknown table id"))
      case Some(list) =>
        list.dropRight(1).foldRight[Either[String, _]](list.last.toTable(gs)) { (next, res) => res.fold(_ => next.toTable(gs), Right(_)) } match {
          case Right(table) => Right(table.asInstanceOf[Table])
          case Left(err) => Left(TableBuildingError(gs.tableId, err))
        }
    }
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

  def singleton[A <: Section with Table : reflect.ClassTag](tableId: Int): TableSupport[A] = {
    val tid = tableId
    new TableSupport[A] {
      def tableId = tid
      def toTable(gs: GroupedSections[Section]) =
        gs.narrow[A].toRight(s"Not a ${reflect.ClassTag[A]}").flatMap { sections =>
          if (sections.tail.isEmpty) Right(sections.head)
          else Left(s"${reflect.ClassTag[A]} supports only 1 section but got ${sections.list.size}")
        }
      def toSections(table: A) = GroupedSections(table)
    }
  }
}
