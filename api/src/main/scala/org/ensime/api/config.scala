// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File
import scala.util.Properties._

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
  (rootDir :: javaHome :: javaSources ::: javaLibs).foreach { f =>
    require(f.exists, "" + f + " is required but does not exist")
  }

  /* Proposed alternatives to the legacy wire format field names */
  def root = rootDir
  val referenceSourceJars =
    (javaSources ++ projects.flatMap(_.librarySources)).toSet

  // some marshalling libs (e.g. spray-json) might not like extra vals
  val modules = projects.map { module => (module.id, module) }.toMap

  def compileClasspath: Set[File] = modules.values.toSet.flatMap {
    m: EnsimeProject => m.libraryJars
  } ++ (if (propOrFalse("ensime.sourceMode")) List.empty else targetClasspath)

  def targetClasspath: Set[File] = modules.values.toSet.flatMap {
    m: EnsimeProject => m.targets
  }

  def allJars: Set[File] = {
    modules.values.flatMap { m =>
      m.libraryJars
    }.toSet
  } ++ javaLibs

  val allTargets: Set[File] =
    projects.flatMap(_.targets).toSet

  def allDocJars: Set[File] = modules.values.flatMap(_.libraryDocs).toSet

  def scalaLibrary: Option[File] = allJars.find(_.getName.startsWith("scala-library"))
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
  sources.foreach(f => require(f.exists, "" + f + " is required but does not exist"))

  def dependencies(implicit config: EnsimeConfig): List[EnsimeProject] =
    depends.toList.map(config.modules)
}
