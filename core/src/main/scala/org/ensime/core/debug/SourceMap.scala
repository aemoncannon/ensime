// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import org.ensime.api._
import org.ensime.indexer.SearchService
import org.ensime.util.ensimefile._

object SourceMap {

  // resolve a filePath e.g. the/package/File.scala (combined out of
  // the class package name and the source file from the debug)
  def fromJdi(jdi: String)(implicit s: SearchService): Option[EnsimeFile] = {
    s.findClasses(jdi).flatMap(_.source).map(EnsimeFile).headOption
  }

  // inverse of fromJdi, convert a user's file into the
  // scala-debugger's representation (the/package/File.scala)
  def toJdi(file: EnsimeFile)(implicit s: SearchService): Option[String] =
    s.findClasses(file).flatMap(_.jdi).headOption

  private def relativeToBase(file: EnsimeFile)(implicit c: EnsimeConfig): Option[String] = file match {
    case ArchiveFile(_, entry) => Option(entry)
    case RawFile(path) =>
      c.projects.flatMap(_.sources)
        .find { root => path.startsWith(root.toPath) }
        .map { root => path.relativize(root.toPath).toString }
  }

  // I don't think we should support this fallback mode, if the
  // indexer doesn't find it, this result is probably nonsense.
  def toJdiWithFallback(file: EnsimeFile)(implicit s: SearchService, c: EnsimeConfig): String =
    (toJdi(file) orElse (relativeToBase(file))).getOrElse {
      throw new IllegalArgumentException(s"unable to resolve $file")
    }

}
