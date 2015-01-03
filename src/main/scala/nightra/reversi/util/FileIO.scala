package nightra.reversi.util

import java.io.PrintWriter
import java.nio.file._

import scala.collection.JavaConverters._
import scala.io.Source
import scalaz.concurrent.Task
import scalaz.syntax.apply._

object FileIO {

  def waitForChange(file: Path): Task[String] = Task {
    val dir = file.toAbsolutePath.getParent
    val watcher = dir.getFileSystem.newWatchService()
    val key = dir.register(watcher, StandardWatchEventKinds.ENTRY_MODIFY)
    val waitKey = watcher.take()

    var events = waitKey.pollEvents()
    while (!events.asScala.map(_.context.asInstanceOf[Path].toAbsolutePath).contains(file.toAbsolutePath)) {
      waitKey.reset()
      events = waitKey.pollEvents()
    }

    val newContent = Source.fromFile(file.toAbsolutePath.toFile).mkString

    waitKey.cancel()
    key.cancel()
    watcher.close()

    newContent
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
