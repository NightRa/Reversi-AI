package nightra.reversi.ui

import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.layout.AnchorPane
import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

import nightra.reversi.ai.{AlphaBeta, GameTreeAI}
import nightra.reversi.model._
import nightra.reversi.util.JavaFXUtil._

class GameUI(boardSize: Int) extends Scene(600, 600) {

  val boardProp: ObjectProperty[Board] = ObjectProperty(Board.initialBoard(boardSize))
  val winner: ObjectProperty[Option[Player]] = mapProp(boardProp)(_.winner)

  winner.onChange((_, _, _) => winner.value match {
    case None => ()
    case Some(winningPlayer) => println(s"The winner is: $winningPlayer")
  })

  def moved(move: Move): Unit = {
    if(boardProp.value.turn == Black){
      boardProp.value.move(move) match {
        case None => ()
        case Some(newBoard) =>
          play(newBoard)
      }
    }
  }

  def aiAsync(board: Board): Task[(Board, Move, Float)] = {
    treeAI(board, 4)
  }

  def imperativeAI(board: Board, depth: Int): Task[(Board,Move,Float)] = {
    val aiTask = Task.delay(AlphaBeta.alphaBeta[Board, (Move, Board)](board)(_.heuristic)(_.possibleMoves, _._2)(_.isTerminal)(_.turn.isMax)(depth))
    // val aiTask = Task.delay(GameTreeAI.reversiMinimax(board, 5))
    Task.fork(aiTask.flatMap {
      case (score, None) => Task.fail(new IllegalStateException("No move for the AI."))
      case (score, Some((move,newBoard))) => Task.now((newBoard, move, score))
    })
  }

  def treeAI(board: Board, depth: Int): Task[(Board,Move,Float)] = {
    val aiTask = Task.delay(GameTreeAI.reversiMinimax(board, depth))
    Task.fork(aiTask.flatMap {
      case None => Task.fail(new IllegalStateException("No move for the AI."))
      case Some((newBoard,move,score)) => Task.now((newBoard, move, score))
    })
  }

  def runAILog(board: Board): Unit = aiAsync(board).runAsync {
    case -\/(exception) => ()
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
      runAILog(board)
  }

  def clickSquare(pos: Position): Unit = {
    moved(Place(pos))
  }

  val game = new BoardUI(boardProp, boardSize, clickSquare)

  root = game
}
