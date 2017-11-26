// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.api

import java.io.File
import java.net._
import java.nio.charset.Charset
import java.nio.file._

import spray.json._
import DeserializationException._

// it would be good to expand this hierarchy and include information
// such as files/dirs, existance, content hints
// (java/scala/class/resource) in the type, validated at construction
// (and can be revalidated at any time)
sealed trait EnsimeFile {
  def uri: URI = this match {
    case RawFile(file)           => file.toUri
    case ArchiveFile(jar, entry) => URI.create(s"jar:${jar.toUri}!${entry}")
  }
  def uriString: String = uri.toASCIIString
}

final case class RawFile(file: Path)                   extends EnsimeFile
final case class ArchiveFile(jar: Path, entry: String) extends EnsimeFile

object EnsimeFile {
  def apply(path: String): EnsimeFile = path match {
    case ArchiveRegex(file, entry) =>
      ArchiveFile(Paths.get(cleanBadWindows(file)), entry)
    case FileRegex(file) => RawFile(Paths.get(cleanBadWindows(file)))
  }
  def apply(path: File): EnsimeFile = RawFile(path.toPath)
  def apply(url: URL): EnsimeFile =
    EnsimeFile(URLDecoder.decode(url.toExternalForm(), "UTF-8"))

  private val ArchiveRegex = "(?:(?:jar:)?file:)?([^!]++)!(.++)".r
  private val FileRegex    = "(?:(?:jar:)?file:)?(.++)".r

  // URIs on Windows can look like /C:/path/to/file, which are malformed
  private val BadWindowsRegex = "/+([^:]+:[^:]+)".r
  private def cleanBadWindows(file: String): String = file match {
    case BadWindowsRegex(clean) => clean
    case other                  => other
  }

  object Implicits {
    implicit val DefaultCharset: Charset = Charset.defaultCharset()
  }

  // clients appreciate a simpler format for files
  implicit val jsWriter: JsWriter[EnsimeFile] = {
    case RawFile(path)  => JsString(path.toString)
    case a: ArchiveFile => JsString(a.uriString)
  }
  implicit val jsReader: JsReader[EnsimeFile] = {
    case JsString(uri) => EnsimeFile(uri)
    case got           => unexpectedJson[EnsimeFile](got)
  }
}
