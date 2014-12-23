package nightra.reversi.util

import scalaz.EphemeralStream

trait UnfoldStep[+A]
case class More[+A](a: A) extends UnfoldStep[A]
case object Done extends UnfoldStep[Nothing]
case object Terminate extends UnfoldStep[Nothing]

object Collections {
  // For square matrices
  // Some iff mat1.size == mat2.size
  def zipWith[A, B, C](mat1: Vector[Vector[A]], mat2: Vector[Vector[B]])(f: (A, B) => C): Option[Vector[Vector[C]]] =
    if (mat1.size != mat2.size)
      None
    else
      Some(Vector.tabulate(mat1.size, mat1.size)((i, j) => f(mat1(i)(j), mat2(i)(j))))

  def to2D(rowSize: Int, index: Int): (Int, Int) = (index / rowSize, index % rowSize)

  // Proof obligation: mat is a square matrix.
  // Damn all these unsafe operations for performance.
  // Collapse a square matrix to a sequence of the point coordinate and the value, in row by row order.
  def collapse[A](mat: Vector[Vector[A]]): Vector[((Int, Int), A)] = {
    val size = mat.size
    Vector.tabulate(size * size) {
      index =>
        val p@(row, col) = to2D(size, index)
        (p, mat(row)(col))
    }
  }

  // Proof obligation: before.size == after.size
  def diff[A](before: Vector[Vector[A]], after: Vector[Vector[A]]): Vector[((Int, Int), A)] = {
    require(before.size == after.size, s"diff requires boards/matricies of equal sizes. before board size: ${before.size}, after board size: ${after.size}")
    collapse(zipWith(before, after)((_, _)).get).collect {
      //                                   ^ unsafe. requires proof of correctness.
      case ((x, y), (piece1, piece2)) if piece1 != piece2 => ((x, y), piece2)
    }
  }

  def unfold[A, B](f: B => Option[(A, B)], start: B): Vector[A] = {
    def go(st: B, v: Vector[A]): Vector[A] = f(st) match {
      case None => v
      case Some((e, st2)) => go(st2, v :+ e)
    }
    go(start, Vector.empty)
  }

  def unfoldMaybe[A, B](f: B => UnfoldStep[(A, B)], start: B): Option[Vector[A]] = {
    def go(st: B, v: Vector[A]): Option[Vector[A]] = f(st) match {
      case Terminate => None
      case Done => Some(v)
      case More((e, st2)) => go(st2, v :+ e)
    }
    go(start, Vector.empty)
  }

  // false - terminate
  // true  - continue
  // Feels just not ok for some reason. ((B,A) => Either[B,B] is isomorphic to (B,A) => (B, Boolean))
  def shortFoldl[A, B](v: Vector[A])(z: B)(f: (B, A) => (B, Boolean)): B = {
    def go(v: Vector[A], index: Int, acc: B): B =
      if (index >= v.size) acc
      else {
        val (value, continue) = f(acc, v(index))
        if (continue) go(v, index + 1, value)
        else value
      }

    go(v, 0, z)
  }

  def collect[A, B](vec: Vector[A])(f: A => Option[B]): Vector[B] = vec.collect(Function.unlift(f))
  def collectStream[A, B](stream: => Stream[A])(f: A => Option[B]): Stream[B] = stream match {
    case Stream.Empty => Stream.empty
    case x #:: xs => f(x) match {
      case None => collectStream(xs)(f)
      case Some(b) => b #:: collectStream(xs)(f)
    }
  }

}
