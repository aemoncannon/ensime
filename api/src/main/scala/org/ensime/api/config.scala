// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File

import scala.util.Properties._
import scala.collection.breakOut

// there is quite a lot of code in this file, when we clean up the
// config file format so that a lot of these hacks are no longer
// needed, we should move the functionality into a higher layer
// RichConfig to keep the API clean.

final case class EnsimeConfig(
    rootDir: File,
    cacheDir: File,
    javaHome: File,
    name: String,
    scalaVersion: String,
    compilerArgs: List[String],
    javaSources: List[File],
    projects: List[EnsimeProject],
    javaLibs: List[File]
) {
  // it would be good if this was implemented as a Validator
  (rootDir :: javaHome :: javaSources ::: javaLibs).foreach { f =>
    require(f.exists, "" + f + " is required but does not exist")
  }

  // it would be good if all the rest of this functionality was moved
  // into typeclasses, instead of polluting the otherwise clean api

  val referenceSourceJars: Set[File] =
    (javaSources ++ projects.flatMap(_.librarySources)).toSet

  val modules: Map[EnsimeProjectId, EnsimeProject] = projects.map { module => (module.id, module) }.toMap

  // FIXME: move these
  val targets: Set[File] = modules.values.flatMap(_.targets)(breakOut)
  // can't be a val because of the implicit
  def classpath: Set[File] = modules.values.flatMap(m => m.classpath(this))(breakOut)

  val allDocJars: Set[File] = modules.values.flatMap(_.libraryDocs)(breakOut)
  val scalaLibrary: Option[File] = modules.values.flatMap(_.libraryJars).find { f =>
    val name = f.getName
    name.startsWith("scala-library") && name.endsWith(".jar")
  }
}

final case class EnsimeProjectId(
  project: String,
  config: String
)

final case class EnsimeProject(
    id: EnsimeProjectId,
    depends: Seq[EnsimeProjectId],
    sources: Set[File],
    targets: Set[File],
    scalacOptions: List[String],
    javacOptions: List[String],
    libraryJars: Set[File],
    librarySources: Set[File],
    libraryDocs: Set[File]
) {
  // see typeclass wishlist in EnsimeConfig

  sources.foreach(f => require(f.exists, "" + f + " is required but does not exist"))

  // TODO: definitely move these, they can't be cached
  def dependencies(implicit config: EnsimeConfig): List[EnsimeProject] =
    depends.toList.map(config.modules)

  def classpath(implicit config: EnsimeConfig): Set[File] = {
    val target = if (propOrFalse("ensime.sourceMode")) Set.empty else targets
    libraryJars ++ target ++ dependencies.flatMap(_.classpath)
  }

}
object EnsimeProject {
  def wholeProject(implicit config: EnsimeConfig): EnsimeProject = new EnsimeProject(
    EnsimeProjectId("wholeProject", "compile"),
    Seq(),
    config.projects.flatMap(_.sources).toSet,
    config.projects.flatMap(_.targets).toSet,
    config.projects.flatMap(_.scalacOptions).distinct,
    config.projects.flatMap(_.javacOptions).distinct,
    config.projects.flatMap(_.libraryJars).toSet,
    config.projects.flatMap(_.librarySources).toSet,
    config.projects.flatMap(_.libraryDocs).toSet
  )
}