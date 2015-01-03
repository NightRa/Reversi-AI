package nightra.reversi.util

import nightra.reversi.interplay.RemoteMove
import nightra.reversi.model._

import scala.util.Try
import scalaz.std.option._
import scalaz.syntax.apply._

object RemoteComputer {
  def parsePlayer(s: String): Option[Player] = s.toLowerCase match {
    case "white" => Some(White)
    case "black" => Some(Black)
    case _ => None
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def parsePosition(s: String): Option[Move] = (s.split(" *, *| ") match {
    case Array(row, col) => (parseInt(row) |@| parseInt(col))(Position.apply)
    case _ => None
  }).map(Move.apply)

  def parseRemoteMove(s: String): Option[RemoteMove] = s.trim.lines.map(_.trim).toList match {
    case List(playerString, positionString) => (parsePlayer(playerString) |@| parsePosition(positionString))(RemoteMove)
    case _ => None
  }

  def showRemoteMove(remoteMove: RemoteMove): String = remoteMove.player.toString + "\r\n" + showMove(remoteMove.move)

  def showMove(move: Move): String = move match{
    case Pass => "-1, -1"
    case Place(Position(row,col)) => s"$row, $col"
  }
}
