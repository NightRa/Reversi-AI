package nightra.reversi.util

import java.io.PrintWriter
import java.nio.file._
import java.util.{TimerTask, Timer}

import nightra.reversi.control.Controller._
import nightra.reversi.control.{InvalidRemoteFormat, InternalError, ExecutionError}

import scala.collection.JavaConverters._
import scala.io.Source
import scalaz.{\/, \/-, -\/}
import scalaz.concurrent.{Future, Task}
import scalaz.syntax.apply._

object FileIO {

  def poll[A](task: PlayResult[Option[A]], delayMillis: Int): PlayResult[A] = playResult(Future.async {
    callback =>
      val timer = new Timer(true) // Daemon thread.

      timer.scheduleAtFixedRate(new TimerTask {
        def run(): Unit = task.run.runAsync {
          case -\/(e@InternalError(_)) =>
            timer.cancel()
            callback(-\/(e))
          case -\/(InvalidRemoteFormat(_)) => ()
          case -\/(e) =>
            System.err.println(s"Error: $e")
          case \/-(None) => () // Continue to run every @delayMillis
          case \/-(Some(a)) =>
            timer.cancel()
            callback(\/-(a))
        }
      }, 0, delayMillis)
  })

  def readFile(file: Path): Task[String] = Task.delay {
    Source.fromFile(file.toAbsolutePath.toFile).mkString
  }

  def writeFile(file: Path, content: String): Task[Unit] =
    createFile(file.toAbsolutePath) *>
      Task.delay {
        val writer = new PrintWriter(file.toAbsolutePath.toFile, "UTF-8")
        writer.write(content)
        writer.close()
      }

  def createFile(file: Path): Task[Unit] = Task.delay {
    Files.createDirectories(file.toAbsolutePath.getParent)
    try Files.createFile(file.toAbsolutePath)
    catch {
      case e: FileAlreadyExistsException => ()
    }
  }

  def localDirectory: Path = {
    Paths.get("").toAbsolutePath
  }

}
