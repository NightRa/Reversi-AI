package nightra.reversi.ai

import Minimax.endBoardValue

object AlphaBeta {
  def alphaBeta[Board, Move](state: Board)(heuristic: Board => Float)(generator: (Board, Boolean) => Stream[Move], toBoard: Move => Board, setTurn: (Board,Boolean) => Board)(terminal: Board => Boolean, winner: Board => Option[Boolean])(max: Boolean)(depth: Int): (Float, Option[Move]) = {
    // Returns none iff bestMin <= bestMax
    def go(state: Board, max: Boolean, stale: Boolean, alpha: Float, beta: Float, depth: Int): (Float, Option[Move]) = {
      if (terminal(state)) {
        (endBoardValue(winner)(state), None)
      } else {
        val possibleMoves: Stream[Move] = generator(state, max)
        if (stale && possibleMoves.isEmpty)
          (endBoardValue(winner)(state), None)
        else if (!stale && possibleMoves.isEmpty) go(setTurn(state,!max), !max, stale = true, alpha, beta, depth)
        else if (depth == 0) {
          // !possibleMoves.isEmpty => can move.
          (heuristic(state), None)
          // Heuristic requires no stale tie.
          // proof of correctness: open moves -> no stale tie possible.
        } else {
          // !possibleMoves.isEmpty
          // god forgive me...
          // Out of time for the homework T_T
          var alpha2 = alpha
          var beta2 = beta
          if (max) {
            var bestMove: Option[Move] = None
            for (child <- possibleMoves) {
              val childValue = go(toBoard(child), !max, stale = false, alpha2, beta2, depth - 1)._1
              if(childValue > alpha2){
                alpha2 = childValue
                bestMove = Some(child)
              }
              if(beta2 <= alpha2)
                return (alpha2, bestMove)
            }
            (alpha2, bestMove)
          } else {
            var bestMove: Option[Move] = None
            for (child <- possibleMoves) {
              val childValue = go(toBoard(child), !max, stale = false, alpha2, beta2, depth - 1)._1
              if(childValue < beta2){
                beta2 = childValue
                bestMove = Some(child)
              }
              if(beta2 <= alpha2)
                return (beta2, bestMove)
            }
            (beta2, bestMove)
          }
        }
      }
    }
    go(state, max, stale = false, alpha = Float.NegativeInfinity, beta = Float.PositiveInfinity, depth)
  }

  /*def alphaBetaLevel[Board, Move](max: Boolean, alpha: Float, beta: Float, bestMove: Move, moves: Vector[Move])(toBoard: Move => Board, search: Board => Float): (Float, Float, Move) =
    moves match {
      case Vector() => (alpha, beta, bestMove)
      case x +: xs =>
        val xValue = search(toBoard(x))
        if (max) {
          val (alpha2, best2) =
            if (xValue > alpha) (xValue, x)
            else (alpha, bestMove)
          if (alpha2 >= beta) (alpha2, beta, best2)
          else alphaBetaLevel(max, alpha2, beta, best2, xs)(toBoard, search)
        } else {
          val (beta2, best2) =
            if (xValue < beta) (xValue, x)
            else (beta, bestMove)
          if (alpha >= beta2) (alpha, beta2, best2)
          else alphaBetaLevel(max, alpha, beta2, best2, xs)(toBoard, search)
        }
    }

  def maximize[Board](board: Board, depth: Int, alpha: Float, beta: Float, terminal: Board => Boolean, heuristic: Board => Float, children: Board => List[Board]): Float = {
    if (depth == 0 || terminal(board)) {
      heuristic(board)
    } else {
      val subtrees = children(board)
      def loop(alpha: Float, l: List[Board]): Float = {
        if (alpha > beta) alpha
        else
          l match {
            case Nil => alpha
            case x :: xs => loop(alpha max (minimize(board, depth, alpha, beta, terminal, heuristic, children)), xs)
          }
      }
      loop(alpha, subtrees)
    }
  }*/
}
