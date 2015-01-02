package nightra.reversi.util

import nightra.reversi.interplay.RemoteMove
import nightra.reversi.model._
import org.specs2.scalaz.Spec
import nightra.reversi.util.RemoteComputer._

class RemoteComputerTest extends Spec {
  "parsePosition" >> {
    parsePosition("1 2") === Some(Place(Position(1,2)))
    parsePosition("1,2") === Some(Place(Position(1,2)))
    parsePosition("1, 2") === Some(Place(Position(1,2)))
    parsePosition("1 , 2") === Some(Place(Position(1,2)))
    parsePosition("-1, -1") === Some(Pass)
    parsePosition("12") === None
  }
  "parsePlayer" >> {
    parsePlayer("WHITE") === Some(White)
    parsePlayer("white") === Some(White)
    parsePlayer("Black") === Some(Black)
    parsePlayer("hello") === None
  }
  "parseRemoteMove" >> {
    parseRemoteMove(
      """White
        |1, 2
      """.stripMargin) === Some(RemoteMove(White, Place(Position(1,2))))
  }
}
