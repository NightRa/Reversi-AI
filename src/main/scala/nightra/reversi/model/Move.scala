package nightra.reversi.model

sealed trait Move
case class Place(at: Position) extends Move
case object Pass extends Move
