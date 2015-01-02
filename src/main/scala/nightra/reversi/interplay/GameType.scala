package nightra.reversi.interplay

import java.io.File

// Int x PlayerType x PlayerType
/*size >= 4 && size is even.*/
case class GameType(boardSize: Int, blackPlayer: Player, whitePlayer: Player)

sealed trait Player
case object HumanPlayer extends Player
case class LocalComputerPlayer(ai: AIType) extends Player
case class RemoteComputerPlayer(file: File) extends Player

// 2 + AIType
sealed trait PlayerType
case object Human extends PlayerType {
  override def toString = "Human"
}
case object LocalComputer extends PlayerType {
  override def toString = "Local computer AI"
}
case object RemoteComputer extends PlayerType {
  override def toString = "Remote computer"
}

// Int + Int = Int x 2
case class AIType(depth: Int /*0-8*/ , searchType: SearchType, implementation: AIImplementation)

sealed trait SearchType
case object Minimax extends SearchType
case object AlphaBeta extends SearchType

sealed trait AIImplementation
case object Tree extends AIImplementation
case object Imperative extends AIImplementation
