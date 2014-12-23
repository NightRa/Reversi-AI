package nightra.reversi.ai

import nightra.reversi.model.{Black, White, Position, Board}

object Heuristic {
  // Positive = Max = Black
  // Negative = Min = White

  def countHeuristic(board: Board): Float =
    100 * (board.blacks - board.whites) / board.pieces

  // Proof obligation: At least one of the players has an open move.
  def mobilityHeuristic(blackOpen: Int, whiteOpen: Int): Float =
    100.0f * (blackOpen - whiteOpen) / (blackOpen + whiteOpen)

  def cornersHeuristic(board: Board): Float = {
    val corners = Vector(board.mat(0)(0), board.mat(0)(board.size - 1), board.mat(board.size - 1)(board.size - 1), board.mat(board.size - 1)(0))
    val blackCorners = corners.count(_.player.exists(_.isBlack))
    val whiteCorners = corners.count(_.player.exists(_.isWhite))
    if(blackCorners + whiteCorners == 0) 0
    else 100.0f * (blackCorners - whiteCorners) / (blackCorners + whiteCorners)
  }

  def nextMoves(board: Board, alreadyOpened: Vector[(Position, Board)], maxOpened: Boolean): (Vector[(Position, Board)], Vector[(Position, Board)]) =
    if (maxOpened)
      (alreadyOpened, board.setTurn(White).possibleMoves)
    else
      (board.setTurn(Black).possibleMoves, alreadyOpened)

  def heuristic(board: Board, alreadyOpened: Vector[(Position, Board)], maxOpened: Boolean): Float = {
    val (blackOpen, whiteOpen) = nextMoves(board, alreadyOpened, maxOpened)
    (countHeuristic(board) + mobilityHeuristic(blackOpen.size, whiteOpen.size) + cornersHeuristic(board)) / 3
  }

}
