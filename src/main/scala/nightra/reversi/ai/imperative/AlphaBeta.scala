package nightra.reversi.ai.imperative

import nightra.reversi.model.{Board, Move}

object AlphaBeta {
  def reversiAlphaBeta(board: Board, depth: Int): (Float, Option[(Move, Board)]) =
    alphaBeta(board, depth, Float.NegativeInfinity, Float.PositiveInfinity)

  def maximize(children: => Stream[(Move, Board)], alpha: Float, beta: Float, eval: (Board, Float, Float) => Float): (Float, (Move, Board)) = {
    def goMaximize(children: Stream[(Move, Board)], max: (Move, Board), alpha: Float): (Float, (Move, Board)) = {
      if (beta <= alpha || children.isEmpty) (alpha, max)
      else {
        val (moveChild, child) = children.head
        val evalChild = eval(child, alpha, beta)
        if (evalChild > alpha) goMaximize(children.tail, (moveChild, child), evalChild)
        else goMaximize(children.tail, max, alpha)
      }
    }
    val res = goMaximize(children.tail, children.head, eval(children.head._2, alpha, beta) max alpha)
    res
  }

  def minimize(children: => Stream[(Move, Board)], alpha: Float, beta: Float, eval: (Board, Float, Float) => Float): (Float, (Move, Board)) = {
    def goMinimize(children: Stream[(Move, Board)], min: (Move, Board), beta: Float): (Float, (Move, Board)) = {
      if (beta <= alpha || children.isEmpty) (beta, min)
      else {
        val (moveChild, child) = children.head
        val evalChild = eval(child, alpha, beta)
        if (evalChild < beta) goMinimize(children.tail, (moveChild, child), evalChild)
        else goMinimize(children.tail, min, beta)
      }
    }
    goMinimize(children.tail, children.head, eval(children.head._2, alpha, beta) min beta)
  }

  def alphaBeta(board: Board, depth: Int, _alpha: Float, _beta: Float): (Float, Option[(Move, Board)]) = {
    if (depth == 0 || board.isTerminal) {
      (board.heuristic, None)
    } else if (board.turn.isMax) {
      val (alpha2, (move, newBoard)) = maximize(board.possibleMoves, _alpha, _beta, (child, alpha, beta) => alphaBeta(child, depth - 1, alpha, beta)._1)
      (alpha2, Some((move, newBoard)))
    } else {
      // turn is min.
      val (beta2, (move, newBoard)) = minimize(board.possibleMoves, _alpha, _beta, (child, alpha, beta) => alphaBeta(child, depth - 1, alpha, beta)._1)
      (beta2, Some((move, newBoard)))
    }
  }
}
