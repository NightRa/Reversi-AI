package nightra.reversi.ui

import nightra.reversi.model._

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.layout.Region

object ReversiPiece {
  private val nonePieceStyle = "radius: 0; "
  private val whitePieceStyle = "-fx-background-color: radial-gradient(radius 100%, white .4, gray .9, darkgray 1); "
  private val blackPieceStyle = "-fx-background-color: radial-gradient(radius 100%, white .0, black .6); "
  private val tileStyle = "-fx-background-radius: 1000em; -fx-background-insets: 5"
  private val seethrough = "-fx-opacity: 0.3; "
}

class ReversiPiece(piece: ObjectProperty[SquareState]) extends Region {
  import ReversiPiece._

  style <== (
    when(piece === WhiteSquareState) choose whitePieceStyle otherwise
      (when(piece === BlackSquareState) choose blackPieceStyle otherwise
        (when(piece === OpenSquareState(White)) choose (whitePieceStyle + seethrough) otherwise
          (when(piece === OpenSquareState(Black)) choose (blackPieceStyle + seethrough) otherwise nonePieceStyle)))
    ) + tileStyle

  prefWidth = 180
  prefHeight = 180
  mouseTransparent = true
}
