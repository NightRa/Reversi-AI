package nightra.reversi.model

sealed trait Move
case class Place(at: Position) extends Move
case object Pass extends Move
object Move {
  def apply(pos: Position): Move = pos match {
    case Position(-1, -1) => Pass
    case _ => Place(pos)
  }
}
