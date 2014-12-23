package nightra.reversi.ai

object Minimax {
  // returns the calculated value of the state + the chosen move.
  // if no moves available, returns none.
  // winner: Given a board, assume it's the end of the game,
  //         Returns None if there's a tie
  //         returns Some true  if Max won
  //         returns Some false if Min won
  // generator: True => generate for the Max player
  //            False => generate for the Min player.
  // heuristic: takes the current state, and the already calculated possible moves, for the given player.
  //            proof obligation: the calculated moves must equal generator(state,max)
  def minimax[Board, Move](state: Board)(heuristic: (Board, Vector[Move], Boolean) => Float)(generator: (Board, Boolean) => Vector[Move], toBoard: Move => Board, setTurn: (Board, Boolean) => Board)(terminal: Board => Boolean, winner: Board => Option[Boolean])(max: Boolean)(depth: Int): (Float, Option[Move]) = {
    def go(state: Board, max: Boolean, stale: Boolean, depth: Int): (Float, Option[Move]) = {
      if (terminal(state)) {
        (endBoardValue(winner)(state), None)
      } else {
        val possibleMoves: Vector[Move] = generator(state, max)
        if (stale && possibleMoves.isEmpty) (endBoardValue(winner)(state), None)
        else if (!stale && possibleMoves.isEmpty) go(setTurn(state, !max), !max, stale = true, depth)
        else if (depth == 0) {
          // !possibleMoves.isEmpty => can move.
          (heuristic(state, possibleMoves, max), None)
          // Heuristic requires no stale tie.
          // proof of correctness: open moves -> no stale tie possible.
        } else {
          // !possibleMoves.isEmpty
          val childrenBoards: Vector[(Move, Float)] = possibleMoves.map(move => (move, go(toBoard(move), !max, stale = false, depth - 1)._1))
          // proposition: forall a,f: size(a.map(f)) = size(a)
          val (move, value) =
            if (max) {
              childrenBoards.maxBy(_._2)
              //             ^ unsafe. correctness: childrenBoards is not empty.
            } else {
              childrenBoards.minBy(_._2)
              //             ^ unsafe. correctness: childrenBoards is not empty.
            }
          (value, Some(move))
        }
      }
    }
    go(state, max, stale = false, depth)
  }

  def endBoardValue[Board](winner: Board => Option[Boolean]): Board => Float =
    board => winner(board) match {
      case None => 0
      case Some(true) => Float.PositiveInfinity
      case Some(false) => Float.NegativeInfinity
    }
}
