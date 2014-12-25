package nightra.reversi.ai.tree

import nightra.reversi.ai.tree.GameTree._
import nightra.reversi.model.{Board, Move}
import nightra.reversi.util.Streams._

import scalaz.std.anyVal._
import scalaz.{OneAnd, Order, Tree}

object TreeMinimax {
  case class OptimalPath[A](chosenLeaf: A, path: OneAnd[Stream,A])

  def minimax[A: Order](tree: => Tree[A])(max: A => Boolean): OptimalPath[A] =
    if (tree.subForest.isEmpty) OptimalPath(tree.rootLabel, OneAnd[Stream,A](tree.rootLabel,Stream.empty))
    else {
      def subPaths: Stream[OptimalPath[A]] = tree.subForest.map(s => minimax(s)(max))
      def extendPath(res: OptimalPath[A]): OptimalPath[A] = OptimalPath(res.chosenLeaf, OneAnd[Stream, A](tree.rootLabel, res.path.head #:: res.path.tail))
      val optimalPath: OptimalPath[A] =
        if (max(tree.rootLabel)) maximum(subPaths)(Order.orderBy(_.chosenLeaf))
        else minimum(subPaths)(Order.orderBy(_.chosenLeaf))
      extendPath(optimalPath)
    }

  def reversiMinimax(root: Board, depth: Int): (Float,Option[(Move, Board)]) = {
    def gameTree: Tree[Board] =
      prune(depth)(generateTree(root))

    def markedTree: Tree[(Board, Float)] =
      gameTree.map(root => (root, root.heuristic))

    val OptimalPath((chosenLeaf, chosenLeafValue), rest) = minimax(markedTree)(_._1.turn.isMax)(Order.orderBy(_._2))
    def movedTo(nextBoard: Board): Move = Board.extractMove(root, nextBoard)
    (chosenLeafValue, rest.tail.headOption.map { case (nextBoard, _) => (movedTo(nextBoard), nextBoard)})
  }
}
