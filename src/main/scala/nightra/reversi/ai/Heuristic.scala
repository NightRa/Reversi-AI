package nightra.reversi.ai

import nightra.reversi.model._

object Heuristic {
  // Positive = Max = Black
  // Negative = Min = White

  def countHeuristic(board: Board): Float =
    100.0f * (board.blacks - board.whites) / board.pieces

  // Proof obligation: At least one of the players has an open move.
  def mobilityHeuristic(board: Board): Float =
    if(board.blackOpen + board.whiteOpen == 0) 0
    else 100.0f * (board.blackOpen - board.whiteOpen) / (board.blackOpen + board.whiteOpen)

  def cornersHeuristic(board: Board): Float = {
    val corners = Vector(board.mat(0)(0), board.mat(0)(board.size - 1), board.mat(board.size - 1)(board.size - 1), board.mat(board.size - 1)(0))
    val blackCorners = corners.count(_.player.exists(_.isBlack))
    val whiteCorners = corners.count(_.player.exists(_.isWhite))
    if (blackCorners + whiteCorners == 0) 0
    else 100.0f * (blackCorners - whiteCorners) / (blackCorners + whiteCorners)
  }

  def heuristic(board: Board): Float = {
    if (board.isTerminal) {
      board.winner match {
        case None => 0f
        case Some(Tie) => 0f
        case Some(Winner(Black)) => Float.PositiveInfinity
        case Some(Winner(White)) => Float.NegativeInfinity
      }
    } else {
      (countHeuristic(board) + mobilityHeuristic(board) + cornersHeuristic(board)) / 3
    }
  }

}
