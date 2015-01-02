package nightra.reversi.ui.game

import nightra.reversi.model.SquareState

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.effect._
import scalafx.scene.layout.{Region, StackPane}
import scalafx.scene.paint.Color

class ReversiSquare(onClick: () => Unit, onHover: () => Unit, onHoverEnd: () => Unit, val state: ObjectProperty[SquareState]) extends StackPane {

  val pieceElement = new ReversiPiece(state)

  val glow: InnerShadow = new InnerShadow(10, Color.DodgerBlue)

  val bgElement = new ReversiBG(onClick, () => {
    effect = glow
    onHover()
  }, () => {
    effect = null
    onHoverEnd()
  })

  content = List(bgElement, pieceElement)
}

class ReversiBG(onClick: () => Unit, onHover: () => Unit, onHoverEnd: () => Unit) extends Region {
  style = "-fx-background-color: #87aade"

  effect = new Lighting {
    light = new Light.Distant {
      azimuth = -135
      elevation = 30
    }
  }

  prefHeight = 200
  prefWidth = 200

  onMouseClicked = onClick

  onMouseEntered = onHover

  onMouseExited = onHoverEnd
}
