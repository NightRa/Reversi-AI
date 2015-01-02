package nightra.reversi.model

import org.scalacheck.Gen
import org.specs2.execute.Results
import org.specs2.matcher.Matchers
import org.specs2.scalaz.Spec
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.apply._

class PositionSpec extends Spec with Matchers {
  val boardSizes = Gen.choose(2, 6).map(_ * 2)
  def boardPosition(size: Int) = (Gen.choose(0, size - 1) |@| Gen.choose(0, size - 1))(Position.apply).map(p => (size, p))
  val positions: Gen[(Int, Position)] = boardSizes.flatMap(boardPosition)

  "# Position:" >> {
    "## Neighbours" should {
      "return positions around the given position" in {
        forAll(positions) {
          case (size, pos) => pos.neighbors(size).forall {
            neighbor =>
              val (dx, dy) = neighbor - pos
              /*
              * math.abs(dx) <= 1
              * math.abs(dy) <= 1
              * neighbor != pos
              **/
              (math.abs(dx) must be_<=(1)) and
                (math.abs(dy) must be_<=(1)) and
                (neighbor must be_!=(pos))
          }
        }
      }
      "return positions which are all in bounds" in {
        forAll(positions) {
          case (size, pos) => pos.neighbors(size).forall {
            neighbor => neighbor.inBounds(size)
          }
        }
      }
      "pass the unit tests:" >> {
        val pos = Position(0, 3)
        "unit test 1 - neighbors" >> {
          val expected = Set(Position(0, 2), Position(1, 2), Position(1, 3), Position(1, 4), Position(0, 4))
          val actual = pos.neighbors(6).toSet
          actual === expected
        }
      }
    }

    "## inBounds" should {
      "return if a position is in bounds" in {
        forAll(positions){
          case (size,pos) => pos.inBounds(size)
        }
      }
      "pass unit test 1" in {
        import Results._
        val pos = Position(0, 3)
        pos.inBounds(6) and
          (Results toResult !pos.inBounds(3))
      }
    }
  }


}
