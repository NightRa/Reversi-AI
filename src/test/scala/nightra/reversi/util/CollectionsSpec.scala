package nightra.reversi.util

import nightra.reversi.model.Piece.Aliases._
import nightra.reversi.model._
import org.specs2.scalaz.Spec

import nightra.reversi.util.Collections._

class CollectionsSpec extends Spec {
  val boardPlaceUpperLeft = Vector(
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, B, E, E, E, E),
    Vector(E, E, E, B, B, E, E, E),
    Vector(E, E, E, B, W, E, E, E),
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, E, E, E, E, E)
  )
  "# Collections:" >> {
    "Diff" in {
      val board1 = Board.initialBoard(8).mat
      val expected = Set(((2, 3), BlackPiece), ((3, 3), BlackPiece))
      val actual = diff(board1, boardPlaceUpperLeft).toSet
      actual === expected
    }
    "to2D" in {
      to2D(8, 9) ===(1, 1)
    }
    "collapse" in {
      val original = Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6),
        Vector(7, 8, 9)
      )
      val collapsed = collapse(original)
      val expected = Vector(((0, 0), 1), ((0, 1), 2), ((0, 2), 3), ((1, 0), 4), ((1, 1), 5), ((1, 2), 6), ((2, 0), 7), ((2, 1), 8), ((2, 2), 9))
      collapsed === expected
    }
    "unfold" in {
      val expected = Vector(1, 2, 4, 8)
      val actual = Collections.unfold[Int, Int](n => Some((n, n * 2)).filter(_ => n < 10), 1)
      actual === expected
    }
    "unfoldMaybe" in {
      val source = Vector(1,2,3,4,5,3)
      val expected = None
      val actual = Collections.unfoldMaybe[Int,(Int,Int)]({
        case (index,n) =>
          if(index >= source.length)
            Done
          else if(source(index) >= n)
            More((source(index),(source(index),index + 1)))
          else
            Terminate
      },(0,Int.MinValue))
      actual === expected
    }
  }


}
