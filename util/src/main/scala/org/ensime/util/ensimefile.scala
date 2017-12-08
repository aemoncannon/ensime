// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.net._
import java.nio.charset.Charset
import java.nio.file._
import java.util.HashMap

import org.ensime.api._
import org.ensime.util.path._

/**
 * Adds functionality to the EnsimeFile sealed family, without
 * polluting the API with implementation detail.
 */
package ensimefile {

  trait RichEnsimeFile {
    def isJava: Boolean
    def isJar: Boolean
    def isScala: Boolean
    def exists(): Boolean
    def lastModified(): Long
    def canon: EnsimeFile

    /** Direct access contents: not efficient for streaming. */
    def readStringDirect()(implicit cs: Charset): String
    def readAllLines: List[String]

  }

}

package object ensimefile {

  implicit class RichRawFile(val raw: RawFile) extends RichEnsimeFile {
    // PathMatcher is too complex, use http://stackoverflow.com/questions/20531247
    override def isJava: Boolean =
      raw.file.toString.toLowerCase.endsWith(".java")
    override def isJar: Boolean = raw.file.toString.toLowerCase.endsWith(".jar")
    override def isScala: Boolean =
      raw.file.toString.toLowerCase.endsWith(".scala")
    override def exists(): Boolean = raw.file.exists()
    override def lastModified(): Long =
      raw.file.attrs.lastModifiedTime().toMillis
    override def readStringDirect()(implicit cs: Charset): String =
      raw.file.readString()
    override def readAllLines: List[String] = raw.file.readLines()
    override def canon: RawFile             = RawFile(raw.file.canon)
  }

  // most methods require obtaining the Path of the entry, within the
  // context of the archive file, and ensuring that we close the
  // resource afterwards (which is slow for random access)
  implicit class RichArchiveFile(val archive: ArchiveFile)
      extends RichEnsimeFile {
    override def isJava: Boolean  = archive.entry.toLowerCase.endsWith(".java")
    override def isJar: Boolean   = archive.entry.toLowerCase.endsWith(".jar")
    override def isScala: Boolean = archive.entry.toLowerCase.endsWith(".scala")
    override def exists(): Boolean =
      archive.jar.exists() && withEntry(_.exists())
    override def lastModified(): Long =
      archive.jar.attrs.lastModifiedTime().toMillis
    override def readStringDirect()(implicit cs: Charset): String =
      withEntry(_.readString())
    override def readAllLines: List[String] = withEntry(_.readLines())
    override def canon: ArchiveFile =
      ArchiveFile(archive.jar.canon, archive.entry)

    def readBytes(): Array[Byte] = withEntry(_.readBytes())

    private def fileSystem(): FileSystem = FileSystems.newFileSystem(
      URI.create(s"jar:${archive.jar.toUri}"),
      new HashMap[String, String]
    )
    private def withFs[T](action: FileSystem => T): T = {
      val fs = fileSystem()
      try action(fs)
      finally fs.close()
    }
    private def withEntry[T](action: Path => T): T = withFs { fs =>
      action(fs.getPath(archive.entry))
    }

    def fullPath: String = s"${archive.jar}!${archive.entry}"

    /*
    import java.nio.file.attribute._
    import java.nio.file.FileVisitResult
    val printer = new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attr: BasicFileAttributes): FileVisitResult = {
        FileVisitResult.CONTINUE
      }
    }
    Files.walkFileTree(fs.getPath("/"), printer)
   */

  }

  // boilerplate-tastic... Coproduct would be helpful here
  implicit def richEnsimeFile(ensime: EnsimeFile): RichEnsimeFile =
    ensime match {
      case raw: RawFile         => new RichRawFile(raw)
      case archive: ArchiveFile => new RichArchiveFile(archive)
    }
}
