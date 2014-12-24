package nightra.reversi.ai

import nightra.reversi.model.{Move, Board}

import scalaz.{OneAnd, Order, Tree}
import scalaz.Tree._
import scalaz.syntax.order._
import scalaz.std.anyVal._

object GameTreeAI {
  type GameTree = Tree[Board]
  def generateTree(root: Board): Tree[Board] = Tree.unfoldTree(root)(root => (root, () => root.possibleMoves.map(_._2)))
  def prune(depth: Int): GameTree => GameTree =
    tree =>
      if (depth == 0) node(tree.rootLabel, Stream.Empty)
      else node(tree.rootLabel, tree.subForest.map(prune(depth - 1)))

  // Just to make sure the traversal is ephemeral.
  def minimum[A: Order](stream: => Stream[A]): A = {
    require(stream.nonEmpty, "minimum and maximum require a non empty stream.")
    def go(stream: Stream[A], acc: A): A =
      if (stream.isEmpty) acc
      else go(stream.tail, acc min stream.head)
    go(stream.tail, stream.head)
  }

  def maximum[A: Order](stream: => Stream[A]): A = minimum(stream)(Order[A].reverseOrder)

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


  def reversiMinimax(root: Board, depth: Int): Option[(Board, Move, Float)] = {
    def gameTree: GameTree =
      prune(depth)(generateTree(root))

    def markedTree: Tree[(Board, Float)] =
      gameTree.map(root => (root, root.heuristic))

    val OptimalPath((chosenLeaf, chosenLeafValue), rest) = minimax(markedTree)(_._1.turn.isMax)(Order.orderBy(_._2))
    def movedTo(nextBoard: Board): Move = Board.extractMove(root, nextBoard)
    rest.tail.headOption.map { case (nextBoard, _) => (nextBoard, movedTo(nextBoard), chosenLeafValue)}
  }
}
