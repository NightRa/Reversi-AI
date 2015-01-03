package nightra.reversi.ui.game

import nightra.reversi.ai.ReversiAI.AI
import nightra.reversi.ai._
import nightra.reversi.control.{InternalError, ExecutionError}
import nightra.reversi.model._
import nightra.reversi.util.JavaFXUtil._

import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalaz.concurrent.Future

class GameUI(boardSize: Int) extends Scene(600, 600) {
  val ai: AI = ReversiAI.Imperative.alphaBeta(5)

  val boardProp: ObjectProperty[Board] = ObjectProperty(Board.initialBoard(boardSize))
  // val winner: ObjectProperty[Option[Player]] = mapProp(boardProp)(_.winner, "winner")

  var playCallback: Option[Position => Unit] = None

  val play: Future[Position] =
    Future.async[Position] {
      callback =>
        Platform.runLater{
          playCallback = Some(callback)
        }
    }

  def reportWinner(winner: Player): Unit = {
    println(s"The winner is: $winner")
  }

  def reportError(error: ExecutionError): Unit = error match {
    case InternalError(e) => e.printStackTrace()
    case _ => println(s"Error: \r\n$error")
  }

  def clickSquare(pos: Position): Unit = {
    playCallback match {
      case None => ()
      case Some(callback) =>
        if (boardProp.value.place(pos).isDefined) {
          playCallback = None
          callback(pos)
        } else {
          ()
        }
    }
  }

  val game = new BoardUI(boardProp, boardSize, clickSquare)

  root = game
}
