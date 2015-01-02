package nightra.reversi.util

import scala.annotation.tailrec
import scalaz.{Tree, Order}
import scalaz.syntax.order._
import scalaz.Ordering._

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

  def maximum[A: Order](stream: => Stream[A]): A = {
    require(stream.nonEmpty, "minimum and maximum require a non empty stream.")
    def maximumLoop(stream: Stream[A], acc: A): A =
      if (stream.isEmpty) acc
      else maximumLoop(stream.tail, acc max stream.head)
    maximumLoop(stream.tail, stream.head)
  }

  def mapMax[A: Order](stream: => Stream[Stream[A]]): Stream[A] = mapMin(stream)(Order[A].reverseOrder)

  def mapMin[A: Order](stream: => Stream[Stream[A]]): Stream[A] =
    if (stream.isEmpty) Stream.Empty
    else {
      val min = minimum(stream.head)
      min #:: omit(min, stream.tail)
    }

  def omit[A: Order](potentialMax: A, stream: => Stream[Stream[A]]): Stream[A] = {
    if (stream.isEmpty) Stream.Empty
    else {
      minNotLe(potentialMax, stream.head) match {
        case None => omit(potentialMax, stream.tail)
        case Some(biggerMinimum) => biggerMinimum #:: omit(biggerMinimum, stream.tail)
      }
    }
  }

  def minNotLe[A: Order](potentialMax: A, stream: => Stream[A]): Option[A] = {
    @tailrec
    def minNotLeLoop(runningMin: A, s: Stream[A]): Option[A] = s match {
      case Stream.Empty => Some(runningMin)
      case x #:: xs =>
        if (x <= potentialMax) None
        else minNotLeLoop(x min runningMin, xs)
    }
    if (stream.isEmpty) None
    else minNotLeLoop(stream.head, stream.tail)
  }

  // Tree.map violates Ephemerality because Stream.map violates Ephemerality..
  def streamMap[A, B](stream: => Stream[A])(f: A => B): Stream[B] =
    if (stream.isEmpty) Stream.Empty
    else f(stream.head) #:: streamMap(stream.tail)(f)

  def treeMap[A, B](tree: => Tree[A])(f: A => B): Tree[B] = {
    Tree.node(f(tree.rootLabel), streamMap(tree.subForest)(treeMap(_)(f)))
  }

  implicit val floatOrder: Order[Float] = new Order[Float] {
    def order(x: Float, y: Float) = if (x < y) LT else if (x == y) EQ else GT
    override def equal(x: Float, y: Float) = x == y
    override def lessThan(x: Float, y: Float) = x < y
    override def greaterThan(x: Float, y: Float) = x > y
    override def lessThanOrEqual(x: Float, y: Float) = x <= y
    override def greaterThanOrEqual(x: Float, y: Float) = x >= y
  }
}
