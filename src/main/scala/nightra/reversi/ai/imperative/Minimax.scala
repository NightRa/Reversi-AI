package nightra.reversi.ai.imperative

import nightra.reversi.model.{Board, Move}

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
  def minimax[Board, Move](state: Board)(heuristic: Board => Float)(generator: Board => Stream[Move])(toBoard: Move => Board)(terminal: Board => Boolean)(max: Board => Boolean)(depth: Int): (Float, Option[Move]) = {
    def go(state: Board, depth: Int): (Float, Option[Move]) = {
      if (depth == 0 || terminal(state)) {
        (heuristic(state), None)
      } else {
        val possibleMoves: Stream[Move] = generator(state)
        // !possibleMoves.isEmpty
        val childrenBoards: Stream[(Move, Float)] = possibleMoves.map(move => (move, go(toBoard(move), depth - 1)._1))
        // proposition: forall a,f: size(a.map(f)) = size(a)
        val (move, value) =
          if (max(state)) {
            childrenBoards.maxBy(_._2)
            //             ^ unsafe. correctness: childrenBoards is not empty.
          } else {
            childrenBoards.minBy(_._2)
            //             ^ unsafe. correctness: childrenBoards is not empty.
          }
        (value, Some(move))
      }
    }
    go(state, depth)
  }

  def reversiMinimax(board: Board, depth: Int): (Float, Option[(Move,Board)]) =
  minimax(board)(_.heuristic)(_.possibleMoves)(_._2)(_.isTerminal)(_.turn.isMax)(depth)
}
