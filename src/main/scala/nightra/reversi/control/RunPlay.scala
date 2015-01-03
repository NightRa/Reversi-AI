package nightra.reversi.control

import nightra.reversi.control.Controller._
import nightra.reversi.interplay.RemoteMove
import nightra.reversi.model.{Board, Move, Player}

import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalaz.concurrent.{Future, Task}
import scalaz.{-\/, EitherT, \/, \/-}

sealed trait ExecutionError
case class InternalError(exception: Throwable) extends ExecutionError
case class IllegalMove(move: RemoteMove) extends ExecutionError
case class AIError(exception: Exception) extends ExecutionError
case class InvalidRemoteFormat(fileContents: String) extends ExecutionError
case class InvalidRemotePlayerPlayed(remoteMove: RemoteMove) extends ExecutionError

trait PlayerRunner[A] {
  def play: Player => Board => PlayResult[A]

  def selfReport: (Player, A) => PlayResult[Unit]

  def toMove: A => Move
}

object Controller {
  type PlayResult[A] = EitherT[Future, ExecutionError, A]
  def playResult[A](v: Future[ExecutionError \/ A]): PlayResult[A] = EitherT[Future, ExecutionError, A](v)

  def taskToPlayResult[A](task: Task[A]): PlayResult[A] =
    playResult(task.get.map {
      case -\/(e) => -\/(InternalError(e))
      case \/-(a) => \/-(a)
    })

  def run[A](board: Board, playerRunner: PlayerRunner[A], report: (Player, Move) => PlayResult[Unit]): PlayResult[Board] = {
    val player = board.turn
    for {
      result <- playerRunner.play(player)(board) // Here I use the transformer to get the inner value.
      move = playerRunner.toMove(result)
      board <- makeMove(board, player, move)
      _ <- playerRunner.selfReport(player, result) // Could use applicative on the last 2, but Intellij inference fails here, so nah.
      _ <- report(player, move)
    } yield board
  }

  def makeMove(board: Board, player: Player, move: Move): PlayResult[Board] = {
    board.move(move) match {
      case None => playResult(Future.now(-\/(IllegalMove(RemoteMove(player, move)))))
      case Some(newBoard) =>
        playResult(Future.delay(\/-(newBoard))) // Set new board
    }
  }
}
