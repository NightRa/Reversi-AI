package nightra.reversi.ui

import nightra.reversi.ui.game.GameUI
import nightra.reversi.ui.menu.MainMenu

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage

object UI extends JFXApp {

  stage = new PrimaryStage {
    stage =>
    title = "Reversi AI"
    scene = new MainMenu(gameType => {
      scene = new GameUI(gameType.boardSize)
    }, stage)
  }

}
