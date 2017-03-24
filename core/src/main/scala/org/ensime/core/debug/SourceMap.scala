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

}
