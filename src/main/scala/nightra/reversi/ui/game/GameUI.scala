package nightra.reversi.ui.game

import nightra.reversi.ai.ReversiAI.AI
import nightra.reversi.ai._
import nightra.reversi.ai.tree.TreeAI
import nightra.reversi.model._
import nightra.reversi.util.JavaFXUtil._

import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

class GameUI(boardSize: Int) extends Scene(600, 600) {

  val ai: AI = ReversiAI.Imperative.alphaBeta(5)

  val boardProp: ObjectProperty[Board] = ObjectProperty(Board.initialBoard(boardSize))
  val winner: ObjectProperty[Option[Player]] = mapProp(boardProp)(_.winner, "winner")

  winner.onChange((_, _, _) => winner.value match {
    case None => ()
    case Some(winningPlayer) => println(s"The winner is: $winningPlayer")
  })

  def moved(move: Move): Unit = {
    if (boardProp.value.turn == Black) {
      boardProp.value.move(move) match {
        case None => ()
        case Some(newBoard) =>
          play(newBoard)
      }
    }
  }

  def aiAsync(ai: AI, board: Board): Task[(Board, Move, Float)] = {
    val aiTask = Task.delay(ai(board))
    Task.fork(aiTask.flatMap {
      case (score, None) => Task.fail(new IllegalStateException("No move for the AI."))
      case (score, Some((move, newBoard))) => Task.now((newBoard, move, score))
    })
  }

  def runAILog(ai: AI, board: Board): Unit = aiAsync(ai, board).runAsync {
    case -\/(exception) => System.err.println(exception)
    case \/-((newBoard, move, score)) =>
      Platform.runLater {
        println(s"The computer moved to $move")
        println(s"The score is: $score")
        play(newBoard)
      }
  }

  def play(board: Board): Unit = board.turn match {
    case Black =>
      if (board.canPass) {
        println("Pass turn for Black!")
        play(board.passTurn)
      } else {
        boardProp.value = board
      }
    case White =>
      boardProp.value = board
      runAILog(ai,board)
  }

  def clickSquare(pos: Position): Unit = {
    moved(Place(pos))
  }

  val game = new BoardUI(boardProp, boardSize, clickSquare)

  root = game
}
