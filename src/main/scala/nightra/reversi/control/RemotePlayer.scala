package nightra.reversi.control

import java.nio.file.Path

import nightra.reversi.control.Controller.PlayResult
import nightra.reversi.interplay.RemoteMove
import nightra.reversi.model.{Board, Player, Move}
import nightra.reversi.util.{FileIO, RemoteComputer}
import nightra.reversi.control.Controller._

import scalaz.concurrent.Future
import scalaz.{\/, \/-, -\/}
import scalaz.syntax.functor._

class RemotePlayer(path: Path) extends PlayerRunner[Move] {
  def play: Player => Board => PlayResult[Move] =
    player => board => {
      playResult(FileIO.waitForChange(path).fproduct(RemoteComputer.parseRemoteMove).get.map[ExecutionError \/ Move] {

        case -\/(exception) => -\/(InternalError(exception))

        case \/-((content, None)) => -\/(InvalidRemoteFormat(content))

        case \/-((_, Some(remoteMove@RemoteMove(remotePlayer, move)))) =>
          if (player == remotePlayer)
            \/-(move)
          else
            -\/(InvalidRemotePlayerPlayed(remoteMove))
      })
    }

  def selfReport: (Player, Move) => PlayResult[Unit] = {
    case (player, move) =>
      playResult(Future.delay(\/- {
        println(s"The remote computer ($player) moved to $move")
      }))
  }

  def toMove: Move => Move = identity
}

object RemotePlayer{
  def apply(path: Path): RemotePlayer = new RemotePlayer(path)
}
