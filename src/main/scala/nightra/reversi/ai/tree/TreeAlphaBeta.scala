package nightra.reversi.ai.tree

import nightra.reversi.util.Streams._

import scalaz.{OneAnd, Order, Tree}

object TreeAlphaBeta extends TreeAI {

  def maximize[A: Order](tree: => Tree[A]): OptimalPath[A] =
    maximum(maximizeM(tree))

  def minimize[A: Order](tree: => Tree[A]): OptimalPath[A] =
    maximize(tree)(Order[A].reverseOrder)

  def maximizeM[A: Order](tree: => Tree[A]): Stream[OptimalPath[A]] = {
    val root = tree.rootLabel
    if (tree.subForest.isEmpty)
      OptimalPath(root, root) #:: Stream.Empty
    else
      streamMap(mapMin(streamMap(tree.subForest)(t => minimizeM(t))))(extendPath(root))
  }

  def minimizeM[A: Order](tree: => Tree[A]): Stream[OptimalPath[A]] = {
    val root: A = tree.rootLabel
    if (tree.subForest.isEmpty)
      OptimalPath(root, root) #:: Stream.Empty
    else
      streamMap(mapMax(streamMap(tree.subForest)(t => maximizeM(t))))(extendPath(root))
  }

  def extendPath[A](root: A)(res: OptimalPath[A]): OptimalPath[A] =
    OptimalPath(res.chosenLeaf, root)

  def alphaBeta[A: Order](tree: => Tree[A])(max: A => Boolean): OptimalPath[A] =
    if (max(tree.rootLabel))
      maximize(tree)
    else
      minimize(tree)

  def apply[A: Order](tree: => Tree[A])(max: (A) => Boolean) = alphaBeta(tree)(max)
}
