package nightra.reversi.control

import java.nio.file.Path

import nightra.reversi.control.Controller.PlayResult
import nightra.reversi.interplay._
import nightra.reversi.model.Player
import nightra.reversi.model._
import nightra.reversi.ui.game.GameUI
import nightra.reversi.interplay
import nightra.reversi.util.FileIO
import nightra.reversi.util
import nightra.reversi.control.Controller._
import scalafx.application.Platform
import scalaz.concurrent.Task
import scalaz.{Applicative, \/-, -\/}
import scalaz.syntax.applicative._

object Game {
  def startGame(gameType: GameType, gameUI: GameUI): Unit = {
    val blackPlayerRunner = playerRunner(gameType.blackPlayer, gameUI)
    val whitePlayerRunner = playerRunner(gameType.whitePlayer, gameUI)
    clearFileState(gameType.blackPlayer, gameType.whitePlayer).run
    runGame(blackPlayerRunner, whitePlayerRunner, gameType, gameUI.boardProp.value, gameUI)
  }

  def runGame(blackPlayerRunner: PlayerRunner[_], whitePlayerRunner: PlayerRunner[_], gameType: GameType, board: Board, gameUI: GameUI): Unit = {
    val currentPlayer = board.turn
    board.winner match {
      case Some(winner) => Platform.runLater {
        gameUI.reportWinner(winner)
      }
      case None =>
        val playInstructions = currentPlayer match {
          case White => Controller.run(board, whitePlayerRunner, report(gameType.blackPlayer, gameType.whitePlayer))
          case Black => Controller.run(board, blackPlayerRunner, report(gameType.blackPlayer, gameType.whitePlayer))
        }
        playInstructions.run.run match {
          case -\/(executionError) => gameUI.reportError(executionError)
          case \/-(newBoard) =>
            Platform.runLater {
              gameUI.boardProp.value = newBoard
            }
            runGame(blackPlayerRunner, whitePlayerRunner, gameType, newBoard, gameUI)
          // recurse and play :)
        }
    }
  }

  // Existential type, I don't care over what type the runner is, it is quantified inside already.
  def playerRunner(player: interplay.Player, gameUI: GameUI): PlayerRunner[_] = player match {
    case HumanPlayer => HumanRunner(gameUI)
    case LocalComputerPlayer(aiType) => AIPlayer(aiType)
    case RemoteComputerPlayer(path) => RemotePlayer(path)
  }


  def report(black: interplay.Player, white: interplay.Player): (Player, Move) => PlayResult[Unit] = {
    (player, move) =>
      (black, white) match {
        case (RemoteComputerPlayer(_), RemoteComputerPlayer(_)) =>
          doNothing
        case (blackPlayer, RemoteComputerPlayer(file)) =>
          if (player == Black)
            reportMove(RemoteMove(player, move), file)
          else
            doNothing
        case (RemoteComputerPlayer(file), whitePlayer) =>
          if (player == White)
            reportMove(RemoteMove(player, move), file)
          else
            doNothing
        case (_, _) =>
          doNothing
      }
  }

  def doNothing: PlayResult[Unit] = ().point[PlayResult]

  def reportMove(remoteMove: RemoteMove, file: Path): PlayResult[Unit] =
    taskToPlayResult(
      FileIO.writeFile(file, util.RemoteComputer.showRemoteMove(remoteMove))
    )

  def clearFile(file: Path): Task[Unit] =
    FileIO.writeFile(file, "")

  def clearFileState(black: interplay.Player, white: interplay.Player): Task[Unit] = (black, white) match{
    case (RemoteComputerPlayer(file), _) => clearFile(file)
    case (_, RemoteComputerPlayer(file)) => clearFile(file)
    case (_, _) => Task.now(())
  }

}
