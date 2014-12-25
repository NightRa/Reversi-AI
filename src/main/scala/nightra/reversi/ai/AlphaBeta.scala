package nightra.reversi.ai

import nightra.reversi.model.{Move, Board}

object AlphaBeta {
  def alphaBeta[Board, Move](state: Board)(heuristic: Board => Float)(generator: Board => Stream[Move])(toBoard: Move => Board)(terminal: Board => Boolean)(max: Board => Boolean)(depth: Int): (Float, Option[Move]) = {
    // Returns none if bestMin <= bestMax, depth is 0, or the state is terminal.
    def go(state: Board, alpha: Float, beta: Float, depth: Int): (Float, Option[Move]) = {
      if (depth == 0 || terminal(state)) {
        (heuristic(state), None)
      } else {
        val possibleMoves: Stream[Move] = generator(state)
        // !possibleMoves.isEmpty
        // god forgive me...
        // Out of time for the homework T_T
        var alpha2 = alpha
        var beta2 = beta
        if (max(state)) {
          var bestMove: Option[Move] = None
          for (child <- possibleMoves) {
            val childValue = go(toBoard(child), alpha2, beta2, depth - 1)._1
            if (childValue > alpha2) {
              alpha2 = childValue
              bestMove = Some(child)
            }
            if (beta2 <= alpha2)
              return (alpha2, bestMove)
          }
          (alpha2, bestMove)
        } else {
          var bestMove: Option[Move] = None
          for (child <- possibleMoves) {
            val childValue = go(toBoard(child), alpha2, beta2, depth - 1)._1
            if (childValue < beta2) {
              beta2 = childValue
              bestMove = Some(child)
            }
            if (beta2 <= alpha2)
              return (beta2, bestMove)
          }
          (beta2, bestMove)
        }
      }
    }

    go(state, alpha = Float.NegativeInfinity, beta = Float.PositiveInfinity, depth)
  }

  def reversiAlphaBeta(board: Board, depth: Int): (Float, Option[(Move, Board)]) =
    alphaBeta(board)(_.heuristic)(_.possibleMoves)(_._2)(_.isTerminal)(_.turn.isMax)(depth)
}
