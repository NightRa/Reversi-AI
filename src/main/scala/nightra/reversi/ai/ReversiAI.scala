package nightra.reversi.ai

import nightra.reversi.ai.imperative.{AlphaBeta, Minimax}
import nightra.reversi.ai.tree.TreeAI
import nightra.reversi.interplay.AIType
import nightra.reversi.interplay
import nightra.reversi.model.{Move, Board}

object ReversiAI {
  // board => value, move, new state
  type AI = Board => (Float, Option[(Move, Board)])
  object Imperative {
    def minimax: Int => AI = depth => Minimax.reversiMinimax(_, depth)
    def alphaBeta: Int => AI = depth => AlphaBeta.reversiAlphaBeta(_, depth)
  }
  object Tree {
    def minimax: Int => AI = depth => TreeAI.minimax(_, depth)
    def alphaBeta: Int => AI = depth => TreeAI.alphaBeta(_, depth)
  }
  def fromAIType(t: AIType): AI = t match {
    case AIType(depth, interplay.Minimax, interplay.Tree) => Tree.minimax(depth)
    case AIType(depth, interplay.AlphaBeta, interplay.Tree) => Tree.alphaBeta(depth)
    case AIType(depth, interplay.Minimax, interplay.Imperative) => Imperative.minimax(depth)
    case AIType(depth, interplay.AlphaBeta, interplay.Imperative) => Imperative.alphaBeta(depth)
  }
}
