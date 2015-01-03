package nightra.reversi.ui

import nightra.reversi.control.Game
import nightra.reversi.ui.game.GameUI
import nightra.reversi.ui.menu.MainMenu

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalaz.concurrent.Future

object UI extends JFXApp {

  stage = new PrimaryStage {
    stage =>
    title = "Reversi AI"
    scene = new MainMenu(gameType => {
      val gameUI = new GameUI(gameType.boardSize)
      scene = gameUI
      Future(Game.startGame(gameType, gameUI)).runAsync(_ => ())
    }, stage)
  }

}
