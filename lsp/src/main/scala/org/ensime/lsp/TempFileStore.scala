// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.net.URI
import java.nio.file._

import akka.event.slf4j.SLF4JLogging
import org.ensime.api._

import scala.util.{ Success, Try }
import monix.eval.Task

/**
  * A place to unzip files from archives
  */
final case class EnsimeCache(path: Path) extends SLF4JLogging {

  def getFile(path: EnsimeFile): Try[Path] = path match {
    case RawFile(p) => Success(p)
    case ArchiveFile(jar, entry) =>
      Try {
        log.info(s"Extracting $jar!$entry to $rootPath")
        val uri = URI.create(s"jar:${jar.toFile.toURI.toString}")
        val zipFile =
          FileSystems.newFileSystem(uri, new java.util.HashMap[String, String])
        val zipFilePath   = zipFile.getPath(entry)
        val targetPath    = if (entry.startsWith("/")) entry.drop(1) else entry
        val extractedPath = rootPath.resolve(targetPath)

        try {
          Files.createDirectories(extractedPath.getParent)
          Files.copy(zipFilePath,
            extractedPath,
            StandardCopyOption.REPLACE_EXISTING)
        } finally zipFile.close()

        extractedPath
      }
  }
}

object EnsimeCache {

  final case class PathIsNotDirectory(path: Path) {
    def toIllegalArgumentException: IllegalArgumentException =
      new IllegalArgumentException(s"Ensime cache $path is not a directory")
  }

  def fromPath(path: String): Either[PathIsNotDirectory, Task[EnsimeCache]] = {
    val rootPath: Path = FileSystems.getDefault.getPath(path)
    if (Files.exists(rootPath)) {
      if (!Files.isDirectory(rootPath)) {
        Left(PathIsNotDirectory(rootPath))
      } else Right(Task(EnsimeCache(rootPath)))
    } else {
      Right(Task.eval {
        Files.createDirectory(rootPath)
        EnsimeCache(rootPath)
      })
    }
  }
}
