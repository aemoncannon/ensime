// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.File
import java.net._
import java.nio.charset.Charset
import java.nio.file._
import java.util.HashMap

import org.ensime.api._
import org.ensime.util.path._

import scala.collection.{ mutable, Map, Set }
import scala.collection.JavaConverters._
import scala.util.Try

/**
 * Adds functionality to the EnsimeFile sealed family, without
 * polluting the API with implementation detail.
 */
package ensimefile {

  trait RichEnsimeFile {
    def isJava: Boolean
    def isJar: Boolean
    def isScala: Boolean
    def isClass: Boolean
    def exists: Boolean
    def lastModified(): Long
    def canon: EnsimeFile
    def pathWithinArchive: Option[String]

    /** Direct access contents: not efficient for streaming. */
    def readStringDirect()(implicit cs: Charset): String
    def readAllLines: List[String]

    def uri: URI
    def uriString: String = uri.toASCIIString
    def path: Path
    def pathWithOpenFs(): (Option[FileSystem], Path)
    def getTopLevelClassFile: EnsimeFile = {
      import scala.reflect.NameTransformer
      val classPath = path
      val className = classPath.getFileName.toString
      val baseClassName = if (className.contains("$")) {
        NameTransformer.encode(NameTransformer.decode(className).split("\\$")(0)) + ".class"
      } else className

      val uri = classPath.toUri.toString
      //TODO: Can we use Path.getParent?
      val parent = uri.substring(0, uri.length - new URI(className).toASCIIString.length)
      val fileName = new URI(baseClassName).toASCIIString
      val topLevelPath = Paths.get(parent, fileName)
      EnsimeFile(topLevelPath.toString)
    }
  }

}

package object ensimefile {

  object Implicits {
    implicit val DefaultCharset: Charset = Charset.defaultCharset()
  }

  private val ArchiveRegex = "(?:(?:jar:)?file:)?([^!]++)!(.++)".r
  private val FileRegex = "(?:(?:jar:)?file:)?(.++)".r
  def EnsimeFile(path: String): EnsimeFile = path match {
    case ArchiveRegex(file, entry) => ArchiveFile(Paths.get(cleanBadWindows(file)), entry)
    case FileRegex(file) => RawFile(Paths.get(cleanBadWindows(file)))
  }
  def EnsimeFile(file: File): EnsimeFile = RawFile(file.toPath)
  def EnsimeFile(url: URL): EnsimeFile = EnsimeFile(URLDecoder.decode(url.toExternalForm(), "UTF-8"))

  // URIs on Windows can look like /C:/path/to/file, which are malformed
  private val BadWindowsRegex = "/+([^:]+:[^:]+)".r
  private def cleanBadWindows(file: String): String = file match {
    case BadWindowsRegex(clean) => clean
    case other => other
  }

  implicit class RichRawFile(val raw: RawFile) extends RichEnsimeFile {
    // PathMatcher is too complex, use http://stackoverflow.com/questions/20531247
    override def isJava: Boolean = raw.path.toString.toLowerCase.endsWith(".java")
    override def isJar: Boolean = raw.path.toString.toLowerCase.endsWith(".jar")
    override def isScala: Boolean = raw.path.toString.toLowerCase.endsWith(".scala")
    override def isClass: Boolean = raw.path.toString.toLowerCase.endsWith(".class")
    override def exists: Boolean = raw.path.exists
    override def lastModified(): Long = raw.path.attrs.lastModifiedTime().toMillis
    override def readStringDirect()(implicit cs: Charset): String = raw.path.readString()
    override def readAllLines: List[String] = raw.path.readLines()
    override def uri: URI = raw.path.toUri()
    override def canon: RawFile = RawFile(raw.path.canon)
    override val pathWithinArchive = None
    override val path = raw.path
    override val pathWithOpenFs = (None, path)

    def scanGrouped: Map[EnsimeFile, Set[EnsimeFile]] = {
      val results = new mutable.HashMap[EnsimeFile, mutable.Set[EnsimeFile]] with mutable.MultiMap[EnsimeFile, EnsimeFile]
      val rootPath = raw.path
      val fs = Try { FileSystems.newFileSystem(rootPath, null) }.toOption
      val pathToScan = if (raw.isJar) {
        fs.get.getPath("/")
      } else rootPath

      val paths = Files.find(pathToScan, Integer.MAX_VALUE, (path, attrs) => path.toString().toLowerCase().endsWith(".class"))
      paths.iterator().asScala.foreach(p => {
        val file =
          if (raw.isJar) ArchiveFile(rootPath, p.toString)
          else RawFile(p)
        val key = file.getTopLevelClassFile
        val keyExists = key match {
          case RawFile(path) => path.exists
          case ArchiveFile(root, entry) => root.exists && fs.get.getPath(entry).exists
        }
        if (keyExists) {
          results.addBinding(key, file)
        }
      })
      fs.map(_.close())
      results
    }
  }

  // most methods require obtaining the Path of the entry, within the
  // context of the archive file, and ensuring that we close the
  // resource afterwards (which is slow for random access)
  implicit class RichArchiveFile(val archive: ArchiveFile) extends RichEnsimeFile {
    override def isJava: Boolean = archive.entry.toLowerCase.endsWith(".java")
    override def isJar: Boolean = archive.root.toString.toLowerCase.endsWith(".jar")
    override def isScala: Boolean = archive.entry.toLowerCase.endsWith(".scala")
    override def isClass: Boolean = archive.entry.toLowerCase.endsWith(".class")
    override def exists: Boolean = archive.root.exists && withEntry(_.exists())
    override def lastModified(): Long = archive.root.attrs.lastModifiedTime().toMillis
    override def readStringDirect()(implicit cs: Charset): String = withEntry(_.readString())
    override def readAllLines: List[String] = withEntry(_.readLines())
    override def uri: URI = URI.create(s"jar:${archive.root.toUri}!${archive.entry}") // path is null (opaque)
    override def canon: ArchiveFile = ArchiveFile(archive.root.canon, archive.entry)
    override val pathWithinArchive = if (uriString.startsWith("jar") || uriString.startsWith("zip")) {
      Some(archive.entry)
    } else {
      None
    }

    override def path = {
      withFs(fs => fs.getPath(archive.entry))
    }

    override def pathWithOpenFs() = {
      val fs = fileSystem()
      (Some(fs), fs.getPath("/"))
    }

    def readBytes(): Array[Byte] = withEntry(_.readBytes())

    //TODO: How do we work with src.zip from jdk?
    private def fileSystem(): FileSystem = FileSystems.newFileSystem(
      URI.create(s"jar:${archive.root.toUri}"),
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

    def fullPath: String = s"${archive.root}!${archive.entry}"

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
