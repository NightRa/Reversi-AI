package nightra.reversi.util

class Lazy[A](thunk: => A) {
  lazy val get = thunk
}

object Lazy {
  def apply[A](thunk: => A): Lazy[A] = new Lazy(thunk)
}
