package nightra.reversi.model

import org.specs2.scalaz.Spec
import nightra.reversi.model.Piece.Aliases._

class BoardSpec extends Spec {
  val boardMove1 = Board(Vector(
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, B, E, E, E, E),
    Vector(E, E, E, B, B, E, E, E),
    Vector(E, E, E, B, W, E, E, E),
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, E, E, E, E, E),
    Vector(E, E, E, E, E, E, E, E)
  ), size = 8, blacks = 4, pieces = 5, stale = false, turn = White)
  "# Board:" >> {
    "Initial Board" >> {
      "initBoard(8)" >> {
        val expected = Board(Vector(
          Vector(E, E, E, E, E, E, E, E),
          Vector(E, E, E, E, E, E, E, E),
          Vector(E, E, E, E, E, E, E, E),
          Vector(E, E, E, W, B, E, E, E),
          Vector(E, E, E, B, W, E, E, E),
          Vector(E, E, E, E, E, E, E, E),
          Vector(E, E, E, E, E, E, E, E),
          Vector(E, E, E, E, E, E, E, E)
        ), size = 8, blacks = 2, pieces = 4, stale = false, turn = Black)
        val actual = Board.initialBoard(8)
        actual === expected
      }
    }
    "place 1" >> {
      val initial = Board.initialBoard(8)
      val actual = initial.place(Position(2, 3))
      val expectedFlipped = Vector(Position(3,3))
      val expected = Some((boardMove1,expectedFlipped))
      actual === expected
    }
    "place 2" >> {
      val initial = boardMove1
      val actual = initial.place(Position(3,2))
      val expected = None
      actual === expected
    }
    "place 3" >> {
      val initial = boardMove1
      val actual = initial.place(Position(2,2))
      val expectedBoard = Board(Vector(
        Vector(E, E, E, E, E, E, E, E),
        Vector(E, E, E, E, E, E, E, E),
        Vector(E, E, W, B, E, E, E, E),
        Vector(E, E, E, W, B, E, E, E),
        Vector(E, E, E, B, W, E, E, E),
        Vector(E, E, E, E, E, E, E, E),
        Vector(E, E, E, E, E, E, E, E),
        Vector(E, E, E, E, E, E, E, E)
      ), size = 8, blacks = 3, pieces = 6, stale = false, turn = Black)
      val expectedFlipped = Vector(Position(3,3))
      val expected = Some(expectedBoard,expectedFlipped)
      actual === expected
    }
  }

}
