// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import java.io.File
import java.nio.file.{ Path, Paths }

import scala.collection.breakOut
import scala.collection.JavaConverters._

import com.typesafe.config._
import org.apache.commons.vfs2.FileObject
import org.ensime.api._
import org.ensime.util.file._
import org.ensime.util.path._
import org.ensime.util.ensimefile._
import org.ensime.vfs.`package`.EnsimeVFS

package object richconfig {

  // avoids a pureconfig / ficus dependency...
  def parseServerConfig(c: Config): EnsimeServerConfig =
    EnsimeServerConfig(
      RawFile(Paths.get(c.getString("ensime.config")).canon),
      parseServerImportsConfig(c.getConfig("ensime.imports")),
      c.getBoolean("ensime.explode.on.disconnect"),
      c.getBoolean("ensime.exit"),
      c.getString("ensime.protocol"),
      c.getBoolean("ensime.exitAfterIndex"),
      c.getBoolean("ensime.disableClassMonitoring"),
      c.getInt("ensime.index.batchSize")
    )
  private def parseServerImportsConfig(c: Config): ImportsConfig =
    ImportsConfig(
      c.getBoolean("locals"),
      c.getString("strategy"),
      c.getStringList("groups").asScala.toList,
      c.getStringList("wildcards").asScala.toSet,
      c.getInt("maxIndividualImports"),
      c.getStringList("collapseExclude").asScala.toSet
    )

  implicit class RichEnsimeConfig(private val c: EnsimeConfig) extends AnyVal {
    // doesn't do the transitive lookups
    def classpath: List[File] =
      (targetFiles(c) ::: libraryJarFiles(c)).distinct
    def targets: List[File] = targetFiles(c)

    def referenceSourceJars: Set[File] =
      (javaSourceFiles(c) ++ librarySourceFiles(c))(breakOut)

    def lookup(id: EnsimeProjectId) = c.projects.find(_.id == id).get

    def allDocJars: Set[File] = libraryDocFiles(c).toSet

    def scalaLibrary: Option[File] =
      libraryJarFiles(c).find { f =>
        val name = f.getName
        name.startsWith("scala-library") && name.endsWith(".jar")
      }

    def findProject(f: FileObject)(implicit vfs: EnsimeVFS) = {
      val filePath = f.getName.getPath
      c.projects collectFirst {
        case project
            if (project.sources ++ project.targets)
              .map(rf => vfs.toFileObject(rf.file.toFile).getName.getPath)
              .exists(filePath.startsWith) =>
          project.id
      }
    }

    def findProject(path: Path): Option[EnsimeProjectId] =
      c.projects collectFirst {
        case project
            if (project.sources ++ project.targets).exists(
              f => path.startsWith(f.file)
            ) =>
          project.id
      }
    def findProject(file: EnsimeFile): Option[EnsimeProjectId] = file match {
      case RawFile(file)       => findProject(file)
      case ArchiveFile(jar, _) => findProject(jar)
    }
    def findProject(file: SourceFileInfo): Option[EnsimeProjectId] =
      findProject(file.file)
  }

  implicit class RichEnsimeProject(private val p: EnsimeProject)
      extends AnyVal {
    def dependencies(implicit config: EnsimeConfig): List[EnsimeProject] =
      p.depends.map(config.lookup)

    def classpath(implicit config: EnsimeConfig): List[File] = {
      // may not agree with the build tool (e.g. could put all targets first)
      val files = (p.targets.toList ::: p.libraryJars).map(_.file.toFile)
      files ::: (dependencies.flatMap(_.classpath))
    }

    def scalaSourceFiles: Set[RawFile] = {
      for {
        root     <- p.sources
        filePath <- root.file.tree
        rawFile  = RawFile(filePath)
        if filePath.isFile && rawFile.isScala
      } yield rawFile
    }.toSet
  }

  private def targetFiles(c: EnsimeConfig): List[File] =
    c.projects.flatMap(_.targets).map(_.file.toFile)
  private def libraryJarFiles(c: EnsimeConfig): List[File] =
    c.projects.flatMap(_.libraryJars).map(_.file.toFile)
  private def librarySourceFiles(c: EnsimeConfig): List[File] =
    c.projects.flatMap(_.librarySources).map(_.file.toFile)
  private def libraryDocFiles(c: EnsimeConfig): List[File] =
    c.projects.flatMap(_.libraryDocs).map(_.file.toFile)
  private def javaSourceFiles(c: EnsimeConfig): List[File] =
    c.javaSources.map(_.file.toFile)

}
