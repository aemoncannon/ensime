// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File
import java.nio.file.Files

import scala.collection.JavaConverters._

final case class EnsimeConfig(
    @deprecating("rootDir is no longer used except in testing") rootDir: File,
    cacheDir: File,
    javaHome: File,
    name: String,
    scalaVersion: String,
    @deprecating("each project will have a compiler") compilerArgs: List[String],
    javaSources: List[File],
    projects: List[EnsimeProject]
) {
  lazy val javaRunTime: List[File] = tree(javaHome).filter(_.getName == "rt.jar").toList

  private def tree(file: File): Stream[File] = {
    Files.walk(file.toPath).iterator().asScala.toStream.map(_.toFile)
  }
}

final case class EnsimeProjectId(
  project: String,
  config: String
)

final case class EnsimeProject(
  id: EnsimeProjectId,
  depends: List[EnsimeProjectId],
  sources: Set[File],
  targets: Set[File],
  scalacOptions: List[String],
  javacOptions: List[String],
  libraryJars: List[File],
  librarySources: List[File],
  libraryDocs: List[File]
)

