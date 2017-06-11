// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime

import java.io.File
import Predef.{ any2stringadd => _, _ => _ }
import org.ensime.api._
import org.ensime.util.file._

package object config {

  implicit class RichEnsimeConfig(val c: EnsimeConfig) extends AnyVal {
    // we should really be using NIO instead of strings...
    def find(path: String): Option[EnsimeProject] =
      c.projects.find(_.sources.exists(f => path.startsWith(f.getPath)))
    def find(file: File): Option[EnsimeProject] = find(file.getPath)
    def find(file: EnsimeFile): Option[EnsimeProject] = file match {
      case RawFile(file) => find(file.toFile)
      case ArchiveFile(jar, _) => find(jar.toFile)
    }
    def find(file: SourceFileInfo): Option[EnsimeProject] = find(file.file)

    // def find(file: Either[File, SourceFileInfo]): Option[EnsimeProject] = file match {
    //   case Left(f) => find(f)
    //   case Right(sourceFileInfo) => find(sourceFileInfo)
    // }
  }

  implicit class RichEnsimeModule(val m: EnsimeProject) extends AnyVal {
    def scalaSourceFiles: Set[File] = for {
      root <- m.sources
      file <- root.tree
      if file.isFile && file.isScala
    } yield file

  }

}
