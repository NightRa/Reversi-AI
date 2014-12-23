package nightra.reversi.ui

import nightra.reversi.ai.{AlphaBeta, Heuristic, Minimax}
import nightra.reversi.model.{Player, Position, SquareState, Board}

import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.layout.{StackPane, GridPane, AnchorPane}

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
}

class GameUI(boardSize: Int) extends Scene(600, 600) {

  import JavaFXUtil._

  val boardProp: ObjectProperty[Board] = ObjectProperty(Board.initialBoard(boardSize))
  val tiles: ObjectProperty[Vector[Vector[SquareState]]] = mapProp(boardProp)(_.mobilityBoard)

  val reversiTiles: GridPane = {
    val board = new GridPane
    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } {
      val square = new ReversiSquare(() => {
        boardProp.value.place(Position(i, j)) match {
          case None => ()
          case Some(newBoard) =>
            boardProp.value = newBoard
            AlphaBeta.alphaBeta(newBoard)(Heuristic.heuristic)(Board.generator, _._2, (board,max) => board.setTurn(Player.fromMax(max)))(_.isTerminal, _.winner.map(_.isMax))(false)(5) match {
              case (score, None) => println(s"The winner is: ${newBoard.winner}")
              case (score, Some((pos, computerMoveBoard))) =>
                boardProp.value = computerMoveBoard
                println(s"The computer moved to $pos")
                println(s"The score is: $score")
            }
        }
      }, () => {
        tiles.value = Board.lookaheadBoard(boardProp.value, Position(i, j))
      }, () => {
        tiles.value = boardProp.value.mobilityBoard
      }, mapProp(tiles)(states => states(i)(j)))
      board.add(square, j, i)
    }
    board
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
