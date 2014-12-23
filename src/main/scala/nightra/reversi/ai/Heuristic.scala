package nightra.reversi.ai

import nightra.reversi.model._

import scalaz.EphemeralStream

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

  def heuristic(board: Board): Float = {
    (countHeuristic(board) + mobilityHeuristic(board.blackOpen, board.whiteOpen) + cornersHeuristic(board)) / 3
  }

}
