package nightra.reversi.control

import java.nio.file.Path

import nightra.reversi.control.Controller.PlayResult
import nightra.reversi.interplay.RemoteMove
import nightra.reversi.model.{Board, Player, Move}
import nightra.reversi.util.{FileIO, RemoteComputer}
import nightra.reversi.control.Controller._

import scalaz.concurrent.{Task, Future}
import scalaz.concurrent.Task._
import scalaz.{Functor, \/, \/-, -\/}
import scalaz.syntax.functor._
import scalaz.syntax.applicative._

class RemotePlayer(file: Path, readRate: Int = 500) extends PlayerRunner[Move] {
  def play: Player => Board => PlayResult[Move] =
    player => board => {
      FileIO.poll(checkCurrent(player), readRate)
    }

  def handleMove[A](from: Task[String])(f: RemoteMove => ExecutionError \/ A): PlayResult[A] = {
    playResult(Functor[Task].fproduct(from)(RemoteComputer.parseRemoteMove).get.map[ExecutionError \/ A] {
      case -\/(exception) => -\/(InternalError(exception))
      case \/-((content, None)) => -\/(InvalidRemoteFormat(content))
      case \/-((_, Some(remoteMove))) => f(remoteMove)
    })
  }

  def checkCurrent: Player => PlayResult[Option[Move]] = player => handleMove(FileIO.readFile(file)) {
    case remoteMove@RemoteMove(remotePlayer, move) =>
      if (player == remotePlayer)
        \/-(Some(move))
      else
        \/-(None)
  }


  def selfReport: (Player, Move) => PlayResult[Unit] = {
    case (player, move) =>
      playResult(Future.delay(\/- {
        println(s"The remote computer ($player) moved to $move")
      }))
  }

  def toMove: Move => Move = identity
}

object RemotePlayer {
  def apply(path: Path): RemotePlayer = new RemotePlayer(path)
}
