package nightra.reversi.ui.menu

import java.io.File
import java.nio.file.Paths

import nightra.reversi.interplay
import nightra.reversi.interplay._
import nightra.reversi.model.{Black, Player, White}
import nightra.reversi.ui.menu.ChoosePlayerPanel._
import nightra.reversi.util.JavaFXUtil._

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.{Group, Node, Scene}
import scalafx.stage.{FileChooser, Stage}

// I hate UI.
class MainMenu(play: GameType => Unit, stage: Stage) extends Scene(600, 400) {
  scene =>
  stylesheets = List("style.css")

  val chooseBlackPlayer = new ChoosePlayerPanel(Black, stage) {
    styleClass += "chooseBlack"
  }
  val chooseWhitePlayer = new ChoosePlayerPanel(White, stage) {
    styleClass += "chooseWhite"
  }

  chooseBlackPlayer.prefWidth <== scene.width / 2 - 30
  chooseWhitePlayer.prefWidth <== scene.width / 2 - 30

  val playButton = new Button("Play") {
    styleClass += "playButton"
  }
  playButton.onMouseClicked = () => {
    println(gameType.value)
    play(gameType.value)
  }

  val boardSizeComboBox = new ComboBox[Int](List(4, 6, 8, 10, 12, 14)) {
    selectElem(selectionModel.value, 10)
  }
  val chooseBoardSize = new HBox {
    styleClass += "boardSize"
    val label = new Label("Board size:")
    label.labelFor = boardSizeComboBox
    content = List(label, boardSizeComboBox)
  }


  val boardSize = boardSizeComboBox.selectionModel.value.selectedItemProperty

  val gameType = map3Prop(boardSize, chooseBlackPlayer.chosenPlayer, chooseWhitePlayer.chosenPlayer)(GameType, "gameType")

  val choosePlayers = new HBox {
    hbox =>
    styleClass += "choosePlayerHBox"
    content = List(chooseBlackPlayer, chooseWhitePlayer)
  }

  val lowerMenu = new BorderPane {
    styleClass += "lowerMenu"
    left = chooseBoardSize
    right = playButton
  }

  content = new VBox {
    styleClass += "MainMenu"
    content = List(choosePlayers, lowerMenu)
  }
}

class ChoosePlayerPanel(player: Player, stage: Stage) extends VBox {
  styleClass += "choosePlayerPanel"
  val playerName = new Text(ChoosePlayerPanel.playerName(player)) {
    styleClass += "playerName"
  }
  playerName.alignmentInParent = Pos.Center
  playerName.textAlignment = TextAlignment.Center

  val choosePlayerType = new ComboBox[PlayerType](List(Human, LocalComputer, RemoteComputer)) {
    selectionModel.value.select(Human)
  }
  val playerType = labeled(choosePlayerType, "Player type:")


  val playerTypeProp = choosePlayerType.selectionModel.value.selectedItemProperty
  val choosePlayerProp = mapProp(playerTypeProp)(ChoosePlayerPanel.options(_, stage), "choosePlayerProp")
  val choosePlayerNodeProp = mapProp(choosePlayerProp)(_._1, "choosePlayerNodeProp")
  val chosenPlayer = flattenProp[interplay.Player](mapProp(choosePlayerProp)(_._2, "nestedChosenPlayer"), "chosenPlayer")

  val childrenList = liftObservableList[Node](mapProp(choosePlayerNodeProp)(playerOptions => ObservableBuffer(playerName, playerType, playerOptions), "childrenList"))

  childrenList.onChange {
    content = childrenList
  }
  content = childrenList
}

object ChoosePlayerPanel {
  val defaultFileLocation = new File("Remote.reversi")

  def playerName(player: Player): String = player match {
    case White => "White Player"
    case Black => "Black Player"
  }

  def chooseLocalComputerAI(): (Node, ObjectProperty[interplay.Player]) = {
    val chooseSearchType = new ComboBox[SearchType](List(Minimax, AlphaBeta)) {
      selectionModel.value.select(AlphaBeta)
    }
    val searchType = labeled(chooseSearchType, "Search type:")


    val chooseImplementation = new ComboBox[AIImplementation](List(Tree, Imperative)) {
      selectionModel.value.select(Imperative)
    }
    val implementation = labeled(chooseImplementation, "Implementation:")


    val chooseDepth = new ComboBox[Int](List(0, 1, 2, 3, 4, 5, 6, 7)) {
      selectElem(selectionModel.value, 4) // Damn I hate overloading.
    }
    val depth = labeled(chooseDepth, "Depth:")

    val playerProp = map3Prop[SearchType, AIImplementation, Int, interplay.Player](chooseSearchType.selectionModel.value.selectedItemProperty, chooseImplementation.selectionModel.value.selectedItemProperty, chooseDepth.selectionModel.value.selectedItemProperty)(
      (searchType, implementation, depth) => LocalComputerPlayer(AIType(depth, searchType, implementation)), "playerProp"
    )
    val vbox = new VBox {
      content = List(searchType, implementation, depth)
      styleClass += "choosePlayer"
    }
    (vbox, playerProp)
  }

  def labeled(node: Node, label: String): BorderPane = {
    val labelNode = new Label(label) {
      labelFor = node
    }
    val pane = new BorderPane {
      left = labelNode
      right = node
    }
    pane
  }

  def selectElem[A](singleSelectionModel: SingleSelectionModel[A], e: A): Unit = singleSelectionModel.select(e)

  def chooseRemoteComputerAI(stage: Stage): (Node, ObjectProperty[interplay.Player]) = {
    val file = ObjectProperty[File](defaultFileLocation)
    val fileTextField = new TextField {
      editable = false
      text <== mapProp(file)(_.getName, "absoluteRemoteFilePath")
    }
    val chooseFileLabeled = labeled(fileTextField, "File:")
    val fileChooser = new FileChooser {
      title = "Choose remote file"
      initialDirectory = Paths.get("").toAbsolutePath.toFile
    }
    val chooseFileButton = new Button("Browse") {
      onMouseClicked = () => {
        val chosenFile = fileChooser.showOpenDialog(stage)
        if(chosenFile != null)
          file.value = chosenFile
      }
    }
    val containerNode = new VBox {
      styleClass += "chooseRemoteComputer"
      content = List(chooseFileLabeled, new BorderPane {
        right = chooseFileButton
      })
    }
    val remotePlayer = mapProp[File, interplay.Player](file)(RemoteComputerPlayer, "remotePlayer")
    (containerNode, remotePlayer)
  }

  def options(playerType: interplay.PlayerType, stage: Stage): (Node, ObjectProperty[interplay.Player]) = playerType match {
    case Human => (new Group(), ObjectProperty(HumanPlayer))
    case LocalComputer => chooseLocalComputerAI()
    case RemoteComputer => chooseRemoteComputerAI(stage)
  }
}
