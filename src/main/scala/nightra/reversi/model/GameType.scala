package nightra.reversi.model

// Int x PlayerType x PlayerType
/*size >= 4 && size is even.*/
case class GameType(boardSize: Int, blackPlayer: PlayerType, whitePlayer: PlayerType)

// 2 + AIType
sealed trait PlayerType
case object Human extends PlayerType
case class LocalComputer(ai: AIType) extends PlayerType
case object RemoteComputer extends PlayerType

// Int + Int = Int x 2
sealed trait AIType {
  val depth: Int
  // 0 - 6
}
case class Minimax(depth: Int) extends AIType
case class AlphaBeta(depth: Int) extends AIType
