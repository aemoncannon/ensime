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
    def lastModified(): Long
    def canon: EnsimeFile

    /** Direct access contents: not efficient for streaming. */
    def readStringDirect()(implicit cs: Charset): String
    def readAllLines: List[String]

    def uri(): URI
    def uriString(): String = uri.toASCIIString
  }

}

package object ensimefile {

  object Implicits {
    implicit val DefaultCharset: Charset = Charset.defaultCharset()
  }

  implicit class RichRawFile(val raw: RawFile) extends RichEnsimeFile {
    override def lastModified(): Long = raw.path.attrs.lastModifiedTime().toMillis
    override def readStringDirect()(implicit cs: Charset): String = raw.path.readString()
    override def uri: URI = raw.path.toUri()
    override def canon: RawFile = RawFile(raw.path.canon)
  }

  // most methods require obtaining the Path of the entry, within the
  // context of the archive file, and ensuring that we close the
  // resource afterwards (which is slow for random access)
  implicit class RichArchiveFile(val archive: ArchiveFile) extends RichEnsimeFile {
    override def lastModified(): Long = archive.path.attrs.lastModifiedTime().toMillis
    override def readStringDirect()(implicit cs: Charset): String = withEntry(_.readString())
    override def readAllLines: List[String] = withEntry(_.readLines())
    override def uri: URI = URI.create(s"jar:${archive.jar.toUri}!${archive.entry}") // path is null (opaque)
    override def canon: ArchiveFile = ArchiveFile(archive.jar.canon, archive.entry)

    def readBytes(): Array[Byte] = withEntry(_.readBytes())

    private def fileSystem(): FileSystem = FileSystems.newFileSystem(
      URI.create(s"jar:${archive.path.toUri}"),
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

    def fullPath: String = s"${archive.path}!${archive.entry}"

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
  implicit def richEnsimeFile(ensime: EnsimeFile): RichEnsimeFile = ensime match {
    case raw: RawFile => new RichRawFile(raw)
    case archive: ArchiveFile => new RichArchiveFile(archive)
  }
}

/*
 * In the ENSIME 1.0 API, source files in jars/zips were extracted
 * into .ensime_cache/dep-src/source-jars/
 *
 * This is to support the legacy formats.
 *
 * Will be removed in ENSIME 3.0
 */
@deprecating
class LegacyArchiveExtraction(ensime_cache: Path) {
  import ensimefile._
  import path._

  private val extract = ensime_cache / "dep-src/source-jars"

  def write(file: EnsimeFile): EnsimeFile = file match {
    case archive @ ArchiveFile(jar, path) =>
      val target = extract / path.replaceAll("^[/]+", "")
      val bytes = archive.readBytes()

      val parent = target.getParent
      Files.createDirectories(parent)

      target.write(bytes)
      RawFile(target)

    case ok => ok
  }

  // no read equivalent, it would involve source resolution (not worth it)
}
