package nightra.reversi.model

import nightra.reversi.ai.Heuristic
import nightra.reversi.util.{More, Done, Terminate, Collections}
import scalaz.EphemeralStream
import scalaz.std.tuple._
import scalaz.syntax.enum._

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

  // Proof obligation: inBounds(pos)
  // If valid move, return the board after placement and the flipped positions.
  // Otherwise, none.
  def place(pos: Position): Option[Board] =
    if (!pos.inBounds(size) || mat(pos.row)(pos.column).isPlayer /*Out of bounds, or player*/ ) {
      None
    } else {
      val rows: Vector[Vector[Position]] =
        Collections.collect[(Position, Piece), Vector[Position]](pos.neighbors(size).map(p => (p, unsafeAt(p)) /* neighbours(size) ensures inBounds(neighbor)*/)) {
          case (neighborPos, piece) =>
            val (dx, dy) = neighborPos - pos
            val row: Option[Vector[Position]] =
              piece.player // Must start with a filled square.
                .filter(player => player == turn.opposite) // Must be the opposing player's piece
                .flatMap(color => terminatedPath(dx, dy, neighborPos, color)) // Fill the row starting from the piece.
            row
        }
      val flipped = rows.flatten
      if (flipped.isEmpty) {
        // No pieces flipped => Invalid move.
        None
      } else {
        val flippedMat = flipped.foldLeft(mat)((mat, flip) => mat.updated(flip.row, mat(flip.row).updated(flip.column, turn.piece)))
        val placedMat = flippedMat.updated(pos.row, flippedMat(pos.row).updated(pos.column, turn.piece))
        val newBlacks = turn match {
          case Black => blacks + flipped.size + 1 // new black placed at pos
          case White => blacks - flipped.size
        }
        Some(Board(placedMat, size, newBlacks, pieces + 1, stale = false, turn.opposite))
      }
    }


  private def terminatedPath(dx: Int, dy: Int, start: Position, startColor: Player): Option[Vector[Position]] = Collections.unfoldMaybe[Position, Position]({
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
      if(canPass) Some(passTurn)
      else None
    case Place(pos) => place(pos)
  }

  def canPass: Boolean = !stale && (possibleMoves match {
    case Stream((Pass, _)) => true
    case _ => false
  })

  // returns a lazy Stream of: the taken move, and the new board.
  lazy val possibleMoves: Stream[(Move, Board)] = {
    val allPositions = Stream.tabulate(size * size)(i => Position.tupled(Collections.to2D(size, i)))
    val openPositions = Collections.collectStream(allPositions)(pos => place(pos).map(board => (Place(pos): Move, board)))
    if (!stale && openPositions.isEmpty) Stream((Pass, passTurn))
    else openPositions
  }

  def mobilityBoard: Vector[Vector[SquareState]] = {
    val open = Collections.collectStream(possibleMoves) { case (Place(pos), board) => Some(pos) case _ => None}.toList.toSet
    Vector.tabulate(size, size)((row, col) => if (open(Position(row, col))) OpenSquareState(turn) else mat(row)(col).squareState)
  }

  lazy val blackOpen = turn match {
    case Black => possibleMoves.size
    case White => setTurn(Black).possibleMoves.size
  }

  lazy val whiteOpen = turn match {
    case White => possibleMoves.size
    case Black => setTurn(White).possibleMoves.size
  }

  lazy val heuristic = Heuristic.heuristic(this)

  // Copy to a new board, without the computed game tree
  def clean: Board = copy()

  def isTerminal: Boolean = {
    pieces == size * size || possibleMoves.isEmpty
  }

  def winner: Option[Player] =
    if (!isTerminal) None
    else {
      if (blacks > whites)
        Some(Black)
      else if (whites > blacks)
        Some(White)
      else
        None
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
  def initialBoard(size: Int) = {
    require(size >= 4 && size % 2 == 0, s"initialBoard: A board size should be >= 4 and even, size: $size")
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

}
