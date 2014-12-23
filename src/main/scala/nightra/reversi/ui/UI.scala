package nightra.reversi.ui

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage

object UI extends JFXApp {


  stage = new PrimaryStage {
    title = "Reversi AI"
    scene = new GameUI(8)
  }

}
