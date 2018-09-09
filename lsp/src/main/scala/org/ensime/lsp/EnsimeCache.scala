// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.net.URI
import java.nio.charset.Charset
import java.nio.file._

import akka.event.slf4j.SLF4JLogging
import org.ensime.api._
import org.ensime.util.file._

import scala.util.Try
import monix.eval.Task

/**
 * A place to unzip files from archives
 */
final case class EnsimeCache(rootPath: Path) extends SLF4JLogging {

  /**
    * Gets the path to an [[EnsimeFile]].
    *
    * If the file is a raw file e.g. a source file within the project, returns the source file path
    * If the file is an entry within a jar, the jar is unzipped into the .ensime_cache directory and the path of the file within the .ensime_cache is returned.
    */
  def path(ensimeFile: EnsimeFile): Task[Path] = ensimeFile match {
    case RawFile(p) => Task(p)
    case ArchiveFile(jar, entry) =>
      Task.fromTry {
        Try {
          log.info(s"Extracting $jar!$entry to $rootPath")
          val uri = URI.create(s"jar:${jar.toFile.toURI.toString}")
          val zipFile =
            FileSystems.newFileSystem(uri,
                                      new java.util.HashMap[String, String])
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

  /** Determines if a file is located within the ensime cache directory */
  def contains(file: File): Boolean = {
    file.getAbsolutePath.startsWith(rootPath.toString)
  }
}



object EnsimeCache {

  private[lsp] val Utf8Charset: Charset = Charset.forName("UTF-8")

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

  def fileText(path: Path): Task[String] =
    Task.eval {
      path.toFile
        .readString()(
          EnsimeCache.Utf8Charset
        )
    }

}
