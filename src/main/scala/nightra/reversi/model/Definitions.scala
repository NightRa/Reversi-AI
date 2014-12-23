package nightra.reversi.model

import nightra.reversi.util.Collections

import scalaz.std.vector._
import scalaz.syntax.apply._

sealed trait Player {
  def piece: Piece = this match {
    case Black => BlackPiece
    case White => WhitePiece
  }

  def opposite: Player = this match {
    case Black => White
    case White => Black
  }

  def isMax: Boolean = this match {
    case Black => true
    case White => false
  }

  def isBlack: Boolean = this match {
    case Black => true
    case White => false
  }

  def isWhite: Boolean = this match {
    case Black => false
    case White => true
  }
}
object Player {
  def fromMax(max: Boolean): Player =
    if (max) Black
    else White
}
case object Black extends Player
case object White extends Player

// ---------------------------------------------

// Piece <=> Maybe Player <=> Player + 1
sealed trait Piece {
  def player: Option[Player] = this match {
    case BlackPiece => Some(Black)
    case WhitePiece => Some(White)
    case EmptyPiece => None
  }

  def isPlayer: Boolean = player.isDefined

  def isEmpty: Boolean = !isPlayer

  def squareState: SquareState = this match {
    case BlackPiece => BlackSquareState
    case WhitePiece => WhiteSquareState
    case EmptyPiece => EmptySquareState
  }

  override def toString = this match {
    case BlackPiece => "B"
    case WhitePiece => "W"
    case EmptyPiece => "-"
  }
}
case object BlackPiece extends Piece
case object WhitePiece extends Piece
case object EmptyPiece extends Piece

object Piece {
  object Aliases {
    val E: Piece = EmptyPiece
    val W: Piece = WhitePiece
    val B: Piece = BlackPiece
  }
}

// ---------------------------------------------

case class Position(row: Int, column: Int) {
  def neighbors(boardSize: Int): Vector[Position] = Collections.collect(Vector(-1, 0, 1).tuple(Vector(-1, 0, 1))) {
    case (x, y) =>
      val nextPos = Position(x + row, y + column)
      if (nextPos != this && nextPos.inBounds(boardSize))
        Some(nextPos)
      else
        None
  }

  def inBounds(boardSize: Int): Boolean = row >= 0 && row < boardSize && column >= 0 && column < boardSize

  def -(other: Position): (Int, Int) = (row - other.row, column - other.column)
}

// ---------------------------------------------

case class PositionedPiece(position: Position, piece: Piece)
