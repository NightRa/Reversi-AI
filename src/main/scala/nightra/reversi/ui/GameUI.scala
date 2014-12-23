package nightra.reversi.ui

import nightra.reversi.ai.{AlphaBeta, Minimax}
import nightra.reversi.model._

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.{StackPane, GridPane, AnchorPane}
import scalaz.{\/-, -\/, \/}
import scalaz.concurrent.Task

object JavaFXUtil {
  def mapProp[A, B](prop: ObjectProperty[A])(f: A => B): ObjectProperty[B] = {
    val newProp: ObjectProperty[B] = ObjectProperty(f(prop.value))
    prop.onChange((_, _, newState) => newProp.value = f(newState))
    newProp
  }

  def map2Prop[A, B, C](prop1: ObjectProperty[A], prop2: ObjectProperty[B])(f: (A, B) => C): ObjectProperty[C] = {
    val newProp: ObjectProperty[C] = ObjectProperty(f(prop1.value, prop2.value))
    prop1.onChange((_, _, newState) => newProp.value = f(newState, prop2.value))
    prop2.onChange((_, _, newState) => newProp.value = f(prop1.value, newState))
    newProp
  }

  def merge[A](props: Vector[ObjectProperty[A]]): ObjectProperty[A] = {
    val newProp: ObjectProperty[A] = ObjectProperty(props.head.value)
    props.foreach(prop => prop.onChange((_, _, newState) => newProp.value = newState))
    newProp
  }

  def toBooleanProp(prop: ObjectProperty[Boolean]): BooleanProperty = {
    val newProp: BooleanProperty = BooleanProperty(prop.value)
    prop.onChange((_, _, newState) => newProp.value = newState)
    newProp
  }
}

class GameUI(boardSize: Int) extends Scene(600, 600) {

  import JavaFXUtil._

  val boardProp: ObjectProperty[Board] = ObjectProperty(Board.initialBoard(boardSize))
  val tiles: ObjectProperty[Vector[Vector[SquareState]]] = mapProp(boardProp)(_.mobilityBoard)
  val winner: ObjectProperty[Option[Player]] = mapProp(boardProp)(_.winner)

  val reversiTiles: GridPane = {
    val board = new GridPane
    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } {
      val square = new ReversiSquare(() => {
        moved(Place(Position(i, j)))
      }, () => {
        tiles.value = Board.lookaheadBoard(boardProp.value, Position(i, j))
      }, () => {
        tiles.value = boardProp.value.mobilityBoard
      }, mapProp(tiles)(states => states(i)(j)))
      board.add(square, j, i)
    }
    board
  }

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

  def aiAsync(board: Board): Task[(Float, Move, Board)] = {
    val aiTask = Task.delay(AlphaBeta.alphaBeta[Board, (Move, Board)](board)(_.heuristic)(_.possibleMoves, _._2)(_.isTerminal)(_.turn.isMax)(5))
    Task.fork(aiTask.flatMap {
      case (score, None) => Task.fail(new IllegalStateException("No move for the AI."))
      case (score, Some((move, newBoard))) => Task.now((score, move, newBoard))
    })

  }
  def runAILog(board: Board): Unit = aiAsync(board).runAsync {
    case -\/(exception) => ()
    case \/-((score, move, newBoard)) =>
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

  val game = new StackPane() {
    content = List(reversiTiles)
  }

  AnchorPane.setTopAnchor(game, 0d)
  AnchorPane.setBottomAnchor(game, 0d)
  AnchorPane.setLeftAnchor(game, 0d)
  AnchorPane.setRightAnchor(game, 0d)

  root = new AnchorPane() {
    content = game
    prefHeight <== width
  }
}
