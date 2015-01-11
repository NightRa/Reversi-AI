package nightra.reversi.ui.game

import javafx.scene.control.{ButtonType, Alert}
import javafx.scene.control.Alert.AlertType
import javafx.stage.Modality

import nightra.reversi.control.{ExecutionError, InternalError}
import nightra.reversi.model._
import nightra.reversi.util.Collections.toOption

import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalaz.concurrent.Future

class GameUI(boardSize: Int, returnToMainMenu: () => Unit) extends Scene(600, 600) {
  val boardProp: ObjectProperty[Board] = ObjectProperty(Board.initialBoard(boardSize))

  var playCallback: Option[Position => Unit] = None

  val play: Future[Position] =
    Future.async[Position] {
      callback =>
        Platform.runLater {
          playCallback = Some(callback)
        }
    }

  def reportWinner(winner: Player, blacks: Int, whites: Int): Unit = {
    println(s"The winner is: $winner")
    val alert = new Alert(AlertType.CONFIRMATION)
    alert.setTitle("The game ended")
    alert.setHeaderText(winningMessage(winner, blacks, whites))
    alert.setContentText("Do you want to return to the main menu?")
    alert.getButtonTypes.setAll(ButtonType.YES, ButtonType.NO)
    alert.initModality(Modality.NONE)
    val res = toOption(alert.showAndWait())
    res match {
      case Some(ButtonType.YES) => returnToMainMenu()
      case _ => ()
    }
  }

  def winningMessage(winner: Player, blacks: Int, whites: Int): String = {
    val score = winner match {
      case Black => s"$blacks-$whites"
      case White => s"$whites-$blacks"
    }
    s"$winner won $score"
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
