package nightra.reversi.util

import nightra.reversi.interplay.RemoteMove
import nightra.reversi.model.{Black, White, Position}
import org.specs2.scalaz.Spec
import nightra.reversi.util.RemoteComputer._

class RemoteComputerTest extends Spec {
  "parsePosition" >> {
    parsePosition("1 2") === Some(Position(1,2))
    parsePosition("1,2") === Some(Position(1,2))
    parsePosition("1, 2") === Some(Position(1,2))
    parsePosition("1 , 2") === Some(Position(1,2))
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
      """.stripMargin) === Some(RemoteMove(White, Position(1,2)))
  }
}
