package nightra.reversi.model

import scala.annotation.tailrec

sealed trait Player {
  def piece: Piece
  def opposite: Player

  def isBlack: Boolean
  def isWhite: Boolean = !isBlack
  def isMax: Boolean = isBlack
}
object Player {
  def fromMax(max: Boolean): Player =
    if (max) Black
    else White
}
case object Black extends Player {
  def piece = BlackPiece
  def isBlack = true
  def opposite = White
}
case object White extends Player {
  def piece = WhitePiece
  def isBlack = false
  def opposite = Black
}

// ---------------------------------------------

// Piece <=> Maybe Player <=> Player + 1
sealed trait Piece {
  def player: Option[Player]
  // Oh performance. used in hotspot - Board.place
  def unsafePlayer: Player
  def squareState: SquareState

  def isPlayer: Boolean
  def isEmpty: Boolean = !isPlayer

  def toString: String
}
case object BlackPiece extends Piece {
  def player = Some(Black)
  def unsafePlayer = Black
  def squareState = BlackSquareState
  def isPlayer = true
  override def toString = "B"
}
case object WhitePiece extends Piece {
  def player = Some(White)
  def unsafePlayer = White
  def squareState = WhiteSquareState
  def isPlayer = true
  override def toString = "W"
}
case object EmptyPiece extends Piece {
  def player = None
  def unsafePlayer = throw new IllegalArgumentException("EmptyPiece.unsafePlayer")
  def squareState = EmptySquareState
  def isPlayer = false
  override def toString = "-"
}

object Piece {
  object Aliases {
    val E: Piece = EmptyPiece
    val W: Piece = WhitePiece
    val B: Piece = BlackPiece
  }
}

sealed trait EndGame
case class Winner(player: Player) extends EndGame
case object Tie extends EndGame

// ---------------------------------------------

case class Position(row: Int, column: Int) {

  import Position._

  // This is a performance hotspot.
  def neighbors(boardSize: Int): List[Position] = {
    @tailrec
    def neighborsLoop(index: Int, acc: List[Position]): List[Position] = {
      if (index < deltas.size) {
        val (dx, dy) = deltas(index)
        val pos = Position(row + dx, column + dy)
        if (pos.inBounds(boardSize)) neighborsLoop(index + 1, pos :: acc)
        else neighborsLoop(index + 1, acc)
      } else {
        acc
      }
    }
    neighborsLoop(0, Nil)
  }

  def inBounds(boardSize: Int): Boolean = row >= 0 && row < boardSize && column >= 0 && column < boardSize

  def -(other: Position): (Int, Int) = (row - other.row, column - other.column)
}

object Position {
  val deltas = Array((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
}

// ---------------------------------------------

case class PositionedPiece(position: Position, piece: Piece)
