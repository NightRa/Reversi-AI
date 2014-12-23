package nightra.reversi.model

import nightra.reversi.util.{More, Done, Terminate, Collections}

case class Board private[model](mat: Vector[Vector[Piece]], size: Int, blacks: Int, pieces: Int, turn: Player) {
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
        Some(Board(placedMat, size, newBlacks, pieces + 1, turn.opposite))
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

  // returns a vector of: the taken move, and the new board.
  def possibleMoves: Vector[(Position, Board)] = Collections.collect(Vector.tabulate(size * size)
    (i => (Position.apply _).tupled(Collections.to2D(size, i))))(
      pos => place(pos).map(board => (pos, board))
    )

  def mobilityBoard: Vector[Vector[SquareState]] = {
    val open = possibleMoves.map(_._1).toSet
    Vector.tabulate(size, size)((row, col) => if (open(Position(row, col))) OpenSquareState(turn) else mat(row)(col).squareState)
  }

  def isTerminal: Boolean = pieces == size * size

  def winner: Option[Player] =
    if(blacks > whites)
      Some(Black)
    else if(whites > blacks)
      Some(White)
    else
      None

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
    }, size, 2, 4, Black)
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

  def generator(board: Board, max: Boolean): Vector[(Position, Board)] = board.setTurn(if(max) Black else White).possibleMoves

}
