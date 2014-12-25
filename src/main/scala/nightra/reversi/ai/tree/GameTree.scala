package nightra.reversi.ai.tree

import nightra.reversi.model.Board

import scalaz.Tree
import scalaz.Tree._

object GameTree {
  def generateTree(root: Board): Tree[Board] = Tree.unfoldTree(root)(root => (root, () => root.possibleMoves.map(_._2)))
  def prune[A](depth: Int): Tree[A] => Tree[A] =
    tree =>
      if (depth == 0) node(tree.rootLabel, Stream.Empty)
      else node(tree.rootLabel, tree.subForest.map(prune(depth - 1)))


}
