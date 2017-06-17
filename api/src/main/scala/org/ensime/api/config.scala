// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File

final case class EnsimeConfig(
  @deprecating("rootDir is no longer used except in testing") rootDir: File,
  cacheDir: File,
  javaHome: File,
  name: String,
  scalaVersion: String,
  @deprecating("each project will have a compiler") compilerArgs: List[String],
  javaSources: List[File],
  projects: List[EnsimeProject]
)

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

