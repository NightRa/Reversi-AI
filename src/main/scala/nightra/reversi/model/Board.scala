package nightra.reversi.model

import nightra.reversi.ai.Heuristic
import nightra.reversi.util._
import nightra.reversi.util.Collections._
import nightra.reversi.util.Streams._
import Board._

import scala.annotation.tailrec
import scala.collection.mutable

case class Board private[model](mat: Vector[Vector[Piece]], size: Int, blacks: Int, pieces: Int, stale: Boolean, turn: Player) {
  def whites = pieces - blacks

  def at(pos: Position): Option[Piece] =
    if (!pos.inBounds(size))
      None
    else
      Some(mat(pos.row)(pos.column))

  // Proof obligation: inBounds(pos)
  def unsafeAt(pos: Position): Piece = mat(pos.row)(pos.column)
  //                                   ^ unsafe

  def setTurn(player: Player) = this.copy(turn = player)

  def passTurn: Board = setTurn(turn.opposite).copy(stale = true)

  // This is one of the main hotspots, so I optimized it as much as I could
  // Given a position, returns a list of flipped positions if it was to be placed,
  // And returns the size of the list - the amount of flipped pieces.
  def flippedIfPlaced(pos: Position): List[Position] = {
    val oppositePlayer = turn.opposite
    val oppositePiece = oppositePlayer.piece
    @tailrec
    def flippedLoop(index: Int, acc: List[Position]): List[Position] =
      if (index >= Position.deltas.length) acc
      else {
        val (dx, dy) = Position.deltas(index)
        val neighborPos = Position(pos.row + dx, pos.column + dy)

        if (neighborPos.inBounds(this.size) && unsafeAt(neighborPos) == oppositePiece) {
          val directedRow: List[Position] = terminatedPath(dx, dy, neighborPos, oppositePlayer).getOrElse(Nil)
          flippedLoop(index + 1, directedRow ++ acc)
        } else {
          flippedLoop(index + 1, acc)
        }
      }
    flippedLoop(0, Nil)
  }

  // If valid move, return the board after placement and the flipped positions.
  // Otherwise, none.
  def place(pos: Position): Option[Board] =
    if (!pos.inBounds(size) || mat(pos.row)(pos.column).isPlayer /*Out of bounds, or player*/ ) {
      None
    } else {
      val flipped = flippedIfPlaced(pos)
      if (flipped.isEmpty) {
        // No pieces flipped => Invalid move.
        None
      } else {
        // Updating the matrix each time isn't efficient.
        val flippedMat = flipped.foldLeft(mat)((mat, flip) => mat.updated(flip.row, mat(flip.row).updated(flip.column, turn.piece)))
        val placedMat = flippedMat.updated(pos.row, flippedMat(pos.row).updated(pos.column, turn.piece))
        val newBlacks = turn match {
          // List.size is O(n) - Bad!
          case Black => blacks + flipped.size + 1 // + 1 - new black placed at pos
          case White => blacks - flipped.size // placed flippedSize white pieces, so there are that many less blacks.
        }
        Some(Board(placedMat, size, newBlacks, pieces + 1, stale = false, turn.opposite))
      }
    }


  private def terminatedPath(dx: Int, dy: Int, start: Position, startColor: Player): Option[List[Position]] = unfoldMaybe[Position, Position]({
    case p@Position(x, y) =>
      at(p) match {
        case None => Terminate // Out of the board
        case Some(piece) => piece.player match {
          case None => Terminate // Empty piece
          case Some(player) =>
            if (player == startColor) // same color, continue
              More((p, Position(x + dx, y + dy)))
            else
              Done // got to an opposite color
        }
      }
  }, start)

  def move(move: Move): Option[Board] = move match {
    case Pass =>
      if (canPass) Some(passTurn)
      else None
    case Place(pos) => place(pos)
  }

  def canPass: Boolean = !stale && (possibleMoves match {
    case Stream((Pass, _)) => true
    case _ => false
  })

  // returns a lazy Stream of: the taken move, and the new board.
  def possibleMoves: Stream[(Move, Board)] = {
    val openPositions = collectStream(positions(size))(pos => place(pos).map(board => (Place(pos): Move, board)))
    if (!stale && openPositions.isEmpty) Stream((Pass, passTurn))
    else openPositions
  }

  def mobilityBoard: Vector[Vector[SquareState]] = {
    val open = collectStream(possibleMoves) { case (Place(pos), board) => Some(pos) case _ => None}.toList.toSet
    Vector.tabulate(size, size)((row, col) => if (open(Position(row, col))) OpenSquareState(turn) else mat(row)(col).squareState)
  }

  lazy val (blackOpen, whiteOpen) = turn match {
    case Black => (possibleMoves.size, setTurn(White).possibleMoves.size)
    case White => (setTurn(Black).possibleMoves.size, possibleMoves.size)
  }

  lazy val heuristic = Heuristic.heuristic(this)

  def isTerminal: Boolean = {
    pieces == size * size || possibleMoves.isEmpty
  }

  def winner: Option[EndGame] =
    if (!isTerminal) None
    else {
      if (blacks > whites)
        Some(Winner(Black))
      else if (whites > blacks)
        Some(Winner(White))
      else
        Some(Tie)
    }


  // To be honest, I don't really like that piece of code.
  // Research needs to be done with sub sum types.
  // Also, local sum types would be nice. (as well as 'named' tuples)
  // row polymorphic sum types? - there are already row polymorphic product types. (in some languages)
  // Pattern matches reap down options from the sum type.
  override def toString: String =
    s"size: $size, blacks: $blacks, whites: $whites, pieces: $pieces, turn: $turn" +
      mat.map(_.mkString).mkString("\r\n", "\r\n", "\r\n")
}

object Board {
  def validBoardSize(size: Int): Boolean = size >= 4 && size % 2 == 0

  def initialBoard(size: Int) = {
    require(validBoardSize(size), s"initialBoard: A board size should be >= 4 and even, size: $size")
    val left = size / 2 - 1
    val right = size / 2
    val top = size / 2 - 1
    val bottom = size / 2

    Board(Vector.tabulate(size, size) {
      (row, col) =>
        if ((row == top && col == left) || (row == bottom && col == right))
          WhitePiece
        else if ((row == top && col == right) || (row == bottom && col == left))
          BlackPiece
        else
          EmptyPiece
    }, size, 2, 4, stale = false, Black)
  }

  def lookaheadBoard(board: Board, pos: Position): Vector[Vector[SquareState]] = {
    val tiles = board.mobilityBoard
    board.place(pos) match {
      case None => tiles
      case Some(newBoard) =>
        Vector.tabulate(board.size, board.size) {
          (row, col) =>
            if (row == pos.row && col == pos.column) board.turn.piece.squareState
            else if (tiles(row)(col).isOpen) tiles(row)(col)
            else newBoard.mat(row)(col).squareState
        }
    }
  }

  //p in positions(size) => inBounds(size)
  def positions(size: Int): Stream[Position] =
    Stream.tabulate(size * size)(i => (Position.apply _).tupled(to2D(size, i)))

  def extractMove(before: Board, after: Board): Option[Move] = {
    // I really don't like placing preconditions.
    // I really want Dependant types please!!!! -- No one is really stopping me from using a dependently typed language..
    require(before.size == after.size, "placedPosition requires boards of equal sizes.")
    if (before == after) None
    else {
      val posMaybe = streamFind(positions(before.size))(p => before.unsafeAt(p).isEmpty && !after.unsafeAt(p).isEmpty)
      //                                                           ^ p in positions(size) => inBounds(size) => safe
      Some(posMaybe.fold[Move](Pass)(Place))
    }

  }

  // Can't determine the turn because of passes.
  // stale depends on how one got to this board.
  def fromMatrix(mat: Vector[Vector[Piece]], turn: Player, stale: Boolean): Board = {
    val rowCount = mat.size
    val validMatrix = validBoardSize(rowCount) && mat.forall(row => row.size == rowCount)
    require(validMatrix, "fromMatrix requires a square matrix with a valid board size; >= 4 and even.")

    val boardSize = mat.size
    val blacks = mat.map(_.count(_ == BlackPiece)).sum
    val pieces = mat.map(_.count(!_.isEmpty)).sum
    Board(mat, boardSize, blacks, pieces, stale, turn)
  }
}
