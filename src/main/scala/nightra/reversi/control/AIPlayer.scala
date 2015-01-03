package nightra.reversi.control

import nightra.reversi.ai.ReversiAI
import nightra.reversi.ai.ReversiAI.AI
import nightra.reversi.control.Controller._
import nightra.reversi.interplay._
import nightra.reversi.model.{Move, Board}

import scalaz.{\/-, -\/}
import scalaz.concurrent.Future

class AIPlayer(ai: AI) extends PlayerRunner[(Float, Move)] {
  def play = player => board => playResult(Future {
    ai(board) match {
      case (score, None) =>
        -\/(AIError(new IllegalStateException("No move for the AI.")))
      case (score, Some((move, newBoard))) =>
        \/-(score, move)
    }
  })
  def selfReport = {
    case (player, (score, move)) =>
      playResult(Future.delay(\/- {
        println(s"The computer ($player) moved to $move")
        println(s"The score is: $score")
      }))
  }
  def toMove = _._2
}

object AIPlayer {
  def fromAIType(t: AIType): AI = t match {
    case AIType(depth, Minimax, Imperative) => ReversiAI.Imperative.minimax(depth)
    case AIType(depth, AlphaBeta, Imperative) => ReversiAI.Imperative.alphaBeta(depth)
    case AIType(depth, Minimax, Tree) => ReversiAI.Tree.minimax(depth)
    case AIType(depth, AlphaBeta, Tree) => ReversiAI.Tree.alphaBeta(depth)
  }

  def apply(ai: AIType): AIPlayer = new AIPlayer(fromAIType(ai))
}
