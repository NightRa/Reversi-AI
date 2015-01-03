package nightra.reversi.control

import nightra.reversi.control.Controller.PlayResult
import nightra.reversi.model._
import nightra.reversi.ui.game.GameUI

import scalaz.EitherT._
import scalaz.concurrent.Future
import scalaz.syntax.applicative._

class HumanRunner(gameUI: GameUI) extends PlayerRunner[Move] {
  def play: Player => Board => PlayResult[Move] = player => board => {
    if (board.canPass)
      (Pass: Move).point[PlayResult]
    else
      right[Future, ExecutionError, Move](gameUI.play.map(Place))
  }
  def selfReport: (Player, Move) => PlayResult[Unit] = (_, _) => ().point[PlayResult]
  def toMove: Move => Move = identity
}

object HumanRunner {
  def apply(gameUI: GameUI): HumanRunner = new HumanRunner(gameUI)
}
