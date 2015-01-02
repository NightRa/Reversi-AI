package nightra.reversi.ai.tree

import nightra.reversi.ai.tree.TreeAlphaBeta.extendPath
import nightra.reversi.util.Streams._

import scalaz.{Order, Tree}

object TreeMinimax extends TreeAI {
  def apply[A: Order](tree: => Tree[A])(max: A => Boolean): OptimalPath[A] ={
    val root = tree.rootLabel
    if(tree.subForest.isEmpty) OptimalPath(root,root)
    else {
      if (max(root)) maximum(subPaths(tree)(max))
      else minimum(subPaths(tree)(max))
    }
  }


  def minimax[A: Order](tree: => Tree[A])(max: A => Boolean): OptimalPath[A] = {
    val root = tree.rootLabel
    if (tree.subForest.isEmpty) OptimalPath(root, root)
    else {
      val optimalPath: OptimalPath[A] =
        if (max(root)) maximum(subPaths(tree)(max))
        else minimum(subPaths(tree)(max))
      extendPath(root)(optimalPath)
    }
  }

  def subPaths[A: Order](tree: => Tree[A])(max: A => Boolean): Stream[OptimalPath[A]] = streamMap(tree.subForest)(s => minimax(s)(max))
}
