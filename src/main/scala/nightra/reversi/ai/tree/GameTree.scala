package nightra.reversi.ai.tree

import nightra.reversi.model.{Move, Board}
import nightra.reversi.util.Lazy
import nightra.reversi.util.Streams._

import scalaz.{Order, Tree}
import scalaz.Tree._

object GameTree {
  def generateTree(root: Board): Tree[Board] = Tree.unfoldTree(root)(root => (root, () => root.possibleMoves.map(_._2)))
  def prune[A](depth: Int): Tree[A] => Tree[A] =
    tree =>
      if (depth == 0) node(tree.rootLabel, Stream.Empty)
      else node(tree.rootLabel, tree.subForest.map(prune(depth - 1)))

  def pruneWithHeight[A](depth: Int): Tree[A] => Tree[(A, Int)] =
    tree =>
      if (depth == 0) node((tree.rootLabel,depth), Stream.Empty)
      else node((tree.rootLabel, depth), tree.subForest.map(pruneWithHeight(depth - 1)))
}
