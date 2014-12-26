package nightra.reversi.util

import scalaz.{Tree, Order}
import scalaz.syntax.order._

object Streams {
  // Of course there are the built in ones,
  // But I need them to be ephemeral, and the standard library doesn't provide these as such.

  // This one is tail recursive because of lazyness :) -- That's awesome.
  def collectStream[A, B](stream: => Stream[A])(f: A => Option[B]): Stream[B] = stream match {
    case Stream.Empty => Stream.empty
    case x #:: xs => f(x) match {
      case None => collectStream(xs)(f)
      case Some(b) => b #:: collectStream(xs)(f)
    }
  }

  def streamFind[A](stream: => Stream[A])(p: A => Boolean): Option[A] = stream match {
    case Stream.Empty => None
    case x #:: xs =>
      if (p(x)) Some(x)
      else streamFind(xs)(p)
  }

  // Just to make sure the traversal is ephemeral.
  def minimum[A: Order](stream: => Stream[A]): A = {
    require(stream.nonEmpty, "minimum and maximum require a non empty stream.")
    def minimumLoop(stream: Stream[A], acc: A): A =
      if (stream.isEmpty) acc
      else minimumLoop(stream.tail, acc min stream.head)
    minimumLoop(stream.tail, stream.head)
  }

  def maximum[A: Order](stream: => Stream[A]): A = minimum(stream)(Order[A].reverseOrder)

  // Tree.map violates Ephemerality because Stream.map violates Ephemerality..
  def streamMap[A, B](stream: => Stream[A])(f: A => B): Stream[B] =
    if (stream.isEmpty) Stream.Empty
    else f(stream.head) #:: streamMap(stream.tail)(f)

  def treeMap[A, B](tree: => Tree[A])(f: A => B): Tree[B] = {
    Tree.node(f(tree.rootLabel), streamMap(tree.subForest)(treeMap(_)(f)))
  }

}
