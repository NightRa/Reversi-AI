package nightra.reversi.ui.tree

import nightra.reversi.ai.ReversiAI
import nightra.reversi.ai.ReversiAI.AI
import nightra.reversi.ai.tree.GameTree
import nightra.reversi.model.{Board, Position}
import nightra.reversi.ui.game.BoardUI
import org.abego.treelayout.util.{DefaultConfiguration, DefaultTreeForTreeLayout}
import org.abego.treelayout.{NodeExtentProvider, TreeForTreeLayout, TreeLayout}

import scala.collection.JavaConverters._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.{Pane, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.Text
import scalafx.stage.Stage
import scalaz.Tree

object TreeUI extends JFXApp {
  def numberTree[A](tree: Tree[A], fromIndex: Int): (Int, Tree[(Int, A)]) = {
    val (lastIndex, newSubforest) =
      tree.subForest.foldLeft((fromIndex + 1, Stream.empty[Tree[(Int, A)]])) {
        case ((index, newChildren), child) =>
          val (maxIndex, newChild) = numberTree(child, index)
          (maxIndex, newChild #:: newChildren)
      }

    (lastIndex, Tree.node((fromIndex, tree.rootLabel), newSubforest.reverse))
  }

  def createLayoutTree[A](tree: Tree[A]): DefaultTreeForTreeLayout[(Int, A)] = {
    val (lastIndex, numberedTree) = numberTree(tree, 0)
    val layoutTree = new DefaultTreeForTreeLayout[(Int, A)](numberedTree.rootLabel)
    def addChildrenOfTree(t: Tree[(Int, A)]): Unit =
      if (!t.subForest.isEmpty)
        layoutTree.addChildren(t.rootLabel, t.subForest.map(_.rootLabel): _*)
    def addRecursively(t: Tree[(Int, A)]): Unit = {
      addChildrenOfTree(t)
      t.subForest.foreach(addRecursively)
    }
    addRecursively(numberedTree)
    layoutTree
  }

  def createLayout[A](t: TreeForTreeLayout[A]): TreeLayout[A] = {
    val extentProvider = new NodeExtentProvider[A] {
      def getWidth(treeNode: A): Double = 20
      def getHeight(treeNode: A): Double = 20
    }
    val configuration = new DefaultConfiguration[A](20, 5)
    new TreeLayout[A](t, extentProvider, configuration)
  }

  class TreeDrawing[A](tree: Tree[A], text: A => String, onClick: A => Unit) extends Scene {
    val layoutTree = createLayoutTree(tree)
    val layout = createLayout(layoutTree)

    def computeRectangles(): Vector[StackPane] = layout.getNodeBounds.asScala.toVector.map {
      case ((index, value), rectangle) =>
        val rect = new Rectangle {
          width = rectangle.width
          height = rectangle.height
          arcWidth = 5
          arcWidth = 5
          fill = Color.Orange
          strokeWidth = 1
        }
        val label: Text = new Text(text(value))
        new StackPane {
          layoutX = rectangle.x
          layoutY = rectangle.y
          content = List(rect, label)
          onMouseClicked = () => onClick(value)
        }
    }
    def computeEdges(root: (Int, A)): Vector[Line] = {
      val rectRoot = layout.getNodeBounds.get(root)
      val children = layoutTree.getChildrenList(root).asScala
      val rectChildren = children.map(layout.getNodeBounds.get(_))
      val lines = rectChildren.map(rect => Line(rectRoot.getCenterX, rectRoot.getCenterY, rect.getCenterX, rect.getCenterY))
      lines.toVector ++ children.flatMap(computeEdges)
    }
    val rectangles = computeRectangles()
    val edges = computeEdges(layoutTree.getRoot)

    root = new Pane {
      prefWidth = layout.getBounds.getWidth + 20
      prefHeight = layout.getBounds.getHeight + 20
      translateX = 20
      translateY = 20 // padding
      content = edges ++ rectangles
    }

  }

  val board = Board.initialBoard(8).place(Position(5, 4)).get.place(Position(3,5)).get
  val tree = GameTree.pruneWithHeight(2)(GameTree.generateTree(board))

  def evaluatePosition(board: Board, height: Int, ai: Int => AI): String = {
    ai(height)(board)._1.toInt.toString
  }

  def openBoard(board: Board): Unit = {
    new Stage() {
      scene = new Scene(400, 400) {
        root = new BoardUI(ObjectProperty(board), board.size, _ => ())
      }
    }.show()
  }

  val ai = ReversiAI.Imperative.alphaBeta

  val treeScene = new TreeDrawing[(Board, Int)](tree, {case (b, height) => evaluatePosition(b, height, ai)}, {case (b,height) => openBoard(b)})
  stage = new PrimaryStage {
    scene = treeScene
  }
}
