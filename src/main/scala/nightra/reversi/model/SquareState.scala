package nightra.reversi.model

sealed trait SquareState {
  def piece: Piece = this match{
    case WhiteSquareState => WhitePiece
    case BlackSquareState => BlackPiece
    case EmptySquareState => EmptyPiece
    case OpenSquareState(_) => EmptyPiece
  }

  def isOpen: Boolean = this match{
    case OpenSquareState(_) => true
    case _ => false
  }
}
case object WhiteSquareState extends SquareState
case object BlackSquareState extends SquareState
case object EmptySquareState extends SquareState
case class OpenSquareState(player: Player) extends SquareState
