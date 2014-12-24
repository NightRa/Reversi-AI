package nightra.reversi.ui

import nightra.reversi.model._
import nightra.reversi.util.JavaFXUtil._

import scalafx.beans.property.ObjectProperty
import scalafx.scene.layout.{AnchorPane, GridPane, StackPane}

class BoardUI(boardProp: ObjectProperty[Board], boardSize: Int, squareClicked: Position => Unit) extends AnchorPane {
  val tiles: ObjectProperty[Vector[Vector[SquareState]]] = mapProp(boardProp)(_.mobilityBoard)

  val reversiTiles: GridPane = {
    val board = new GridPane
    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } {
      val pos: Position = Position(i, j)
      val square = new ReversiSquare(
        () => squareClicked(pos),
        () => showLookahead(pos),
        () => {
          // tiles.value = boardProp.value.mobilityBoard
        },
        mapProp(tiles)(states => states(i)(j)))
      board.add(square, j, i)
    }
    board
  }

  def showLookahead(pos: Position): Unit = {
    tiles.value = Board.lookaheadBoard(boardProp.value, pos)
  }

  val game = new StackPane {
    content = reversiTiles
  }

  AnchorPane.setTopAnchor(game, 0d)
  AnchorPane.setBottomAnchor(game, 0d)
  AnchorPane.setLeftAnchor(game, 0d)
  AnchorPane.setRightAnchor(game, 0d)

  content = game

}
