package nightra.reversi.ai

import nightra.reversi.ai.tree.{TreeAI, TreeMinimax}
import nightra.reversi.model._
import org.scalacheck.{Test, Gen}
import org.scalacheck.Test.Parameters
import org.specs2.scalaz.Spec
import scala.language.higherKinds
import scalaz._
import Kleisli._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Prop.forAll
import AITest._

class AITest extends Spec {
  "# Game tree search algorithms" >> {
    "## All algorithms should produce the same result" >> {
      forAll(boards(8), searchDepths) {
        (board, depth) =>
          val (alphaBeta,_) = AlphaBeta.reversiAlphaBeta(board, depth)
          val (minimax,_) = Minimax.reversiMinimax(board,depth)
          val (treeMinimax,_) = TreeAI.minimax(board,depth)
          val (treeAlphaBeta, _) = TreeAI.alphaBeta(board, depth)
          (alphaBeta === minimax) and
            (minimax === treeMinimax) and
            (treeMinimax === treeAlphaBeta)
      }.set(minTestsOk = 5, workers = 2)
    }
  }
}

object AITest {
  def chooseMove: Board => Gen[Board] = board => Gen.oneOf(board.possibleMoves.map(_._2))
  def genBoard(boardSize: Int, length: Int): Gen[Board] = {
    val initialBoard = Board.initialBoard(boardSize)
    // Kind projector FTW!
    // I wrote a blog post about this line of code.
    Category[Kleisli[Gen, ?, ?]].monoid.multiply(kleisli(chooseMove), length)(initialBoard)
  }

  def boards(boardSize: Int): Gen[Board] = Gen.choose(0, boardSize * boardSize - 4).flatMap(genBoard(boardSize, _))
  def searchDepths: Gen[Int] = Gen.choose(0, 4)
  /**
   * Hello there!
   * You are into a story.
   * I'm building a Reversi AI with ephemeral trees in Scala, (inspired by the paper "Why Functional Programming Matters")
   * and I wanted to property test my implementations against each other.
   * The property is that all implementations: Minimax and Alpha-beta pruning should give the same result.
   * How would I go about doing it?
   * So I wanted to generate a board by starting at the initial board, and applying random moves on that board.
   * I have a function possibleMoves :: Board => List[Move]
   * Then I realized I have a more symmetric looking function possibleMoves :: Board => List[Board]
   * With which I can get chooseMove :: Board => Gen[Board]
   * So I figured I wanted to apply bind (>>=/flatMap) n times on the initial board.
   * Something of the form: Int -> m a -> (a -> m a) -> m a
   * There's replicateM :: Int -> m a -> List[m a]
   * but that isn't satisfactory.
   * And I couldn't find that function built into Scalaz.
   * What if we juggle that type around?
   * We get: Int -> (a -> m a) -> (a -> m a)
   * That looks like Kleisli, and as m is a Monad, those are Kleisli Arrows!
   * So I could do something like fold(replicate k f) with the kleisli monoid.
   * Then I thought it looked like a Category, as Kleisli Arrows form a Category.
   * So it's just composing an endomorphism n times with itself!
   * But what's a category with only one object? A Monoid!
   * So we want to compose an arrow of the monoid (in the categorical sense) n times with itself.
   * But what's an arrow of a monoid?
   * It is an element of the monoid!
   * And what is arrow composition?
   * It is the monoid's operation!
   * So we want to compose an arrow n times with itself, which means:
   * x + x + x + ... + x, n times.
   * Which is: You guessed it!
   * It is multiplication of an element of a monoid by a natural number!!!!!!!!
   * We have just reinvented multiplication.
   *
   * Someone mentioned it's like church numerals and addition (the successor function as the operation)
   * It is exactly that too! :)
   *
   * The whole code turned out to be:
   * Category[Kleisli[Gen,?,?]].monoid.multiply(chooseMove,length)(initialBoard)
   *
   * Hope you enjoyed your daily theoretical beauty dose.
   **/

}
