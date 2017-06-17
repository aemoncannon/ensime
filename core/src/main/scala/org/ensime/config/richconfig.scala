// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import java.io.File
import java.nio.file.Path

import scala.collection.breakOut

import org.ensime.api._
import org.ensime.util.file._

package object richconfig {

  implicit class RichEnsimeConfig(val c: EnsimeConfig) extends AnyVal {
    // doesn't do the transitive lookups
    def classpath: List[File] =
      (c.projects.flatMap(_.targets) ::: c.projects.flatMap(_.libraryJars)).distinct
    def targets: List[File] =
      c.projects.flatMap(_.targets)

    def referenceSourceJars: Set[File] =
      (c.javaSources ++ c.projects.flatMap(_.librarySources))(breakOut)

    def lookup(id: EnsimeProjectId) =
      c.projects.find(_.id == id).get

    def allDocJars: Set[File] =
      c.projects.flatMap(_.libraryDocs)(breakOut)

    def scalaLibrary: Option[File] =
      c.projects.flatMap(_.libraryJars).find { f =>
        val name = f.getName
        name.startsWith("scala-library") && name.endsWith(".jar")
      }

    def findProject(path: Path): Option[EnsimeProjectId] = {
      // should use NIO relations instead of string comparison...
      // needs https://github.com/ensime/ensime-server/issues/1791
      c.projects collectFirst {
        case project if project.sources.exists(f => path.startsWith(f.toPath)) => project.id
      }
    }
    def findProject(file: EnsimeFile): Option[EnsimeProjectId] = file match {
      case RawFile(file) => findProject(file)
      case ArchiveFile(jar, _) => findProject(jar)
    }
    def findProject(file: SourceFileInfo): Option[EnsimeProjectId] = findProject(file.file)
  }

  implicit class RichEnsimeProject(val p: EnsimeProject) extends AnyVal {
    def dependencies(implicit config: EnsimeConfig): List[EnsimeProject] =
      p.depends.map(config.lookup)

    def classpath(implicit config: EnsimeConfig): List[File] = {
      // may not agree with the build tool (e.g. could put all targets first)
      p.targets.toList ::: p.libraryJars ::: dependencies.flatMap(_.classpath)
    }

    def scalaSourceFiles: Set[RawFile] = for {
      root <- p.sources
      file <- root.tree // should use NIO
      if file.isFile && file.isScala
    } yield RawFile(file.toPath)
  }

}
