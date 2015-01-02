package nightra.reversi.ai.tree

import nightra.reversi.model.{Move, Board}
import nightra.reversi.util.Lazy
import nightra.reversi.util.Streams._
import GameTree._
import scalaz.{Tree, Order, OneAnd}

case class OptimalPath[A](chosenLeaf: A, chosenChild: A)
object OptimalPath{
  implicit def optimalPathOrder[A: Order]: Order[OptimalPath[A]] = Order.orderBy(_.chosenLeaf)
}

trait TreeAI {
  def apply[A: Order](tree: => Tree[A])(max: A => Boolean): OptimalPath[A]
}

object TreeAI {
  def reversiTreeAI(root: Board, depth: Int)(ai: TreeAI): (Float, Option[(Move, Board)]) = {
    def gameTree: Tree[Board] =
      prune(depth)(generateTree(root))

    def markedTree: Tree[(Board, Lazy[Float])] =
      treeMap(gameTree)(root => (root, Lazy(root.heuristic)))

    val OptimalPath((chosenLeaf, chosenLeafValue), (nextBoard,_)) = ai(markedTree)(_._1.turn.isMax)(Order.orderBy(_._2.get))
    def movedTo(nextBoard: Board): Option[Move] = Board.extractMove(root, nextBoard)
    (chosenLeafValue.get, movedTo(nextBoard).map(move => (move, nextBoard)))
  }
  
  def minimax(root:Board, depth: Int): (Float, Option[(Move, Board)]) = reversiTreeAI(root,depth)(TreeMinimax)
  def alphaBeta(root: Board, depth: Int): (Float, Option[(Move, Board)]) = reversiTreeAI(root,depth)(TreeAlphaBeta)
}
