package scodec.protocols

import org.scalacheck.Gen
import scalaz.Lens
import scalaz.concurrent.Task
import scalaz.stream._

class process1extTest extends ProtocolsSpec {

  case class Point(x: Int, y: Int)
  val lensX = Lens.lensu[Point, Int]((p, x) => p.copy(x = x), _.x)

  "the lens combinator" should {

    "work with all Process1 types" in {
      val a = Point(1, 2)
      val b = Point(3, 4)
      val sources = Seq(
        Process.emitAll(Seq(a, b)).toSource,
        (Process.emit(a) ++ Process.emit(b)).toSource
      )
      forAll (Gen.oneOf(sources)) { (src: Process[Task, Point]) =>
        val inc: Process1[Int, Int] = process1.lift { _ + 1 }
        val result = src.pipe(process1ext.lens(lensX)(inc)).runLog.run
        result shouldBe Vector(Point(2, 2), Point(4, 4))
      }
    }
  }
}
