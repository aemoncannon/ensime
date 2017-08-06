// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

final case class EnsimeConfig(
  @deprecating("rootDir is no longer used except in testing") rootDir: RawFile,
  cacheDir: RawFile,
  javaHome: RawFile,
  name: String,
  scalaVersion: String,
  @deprecating("each project will have a compiler") compilerArgs: List[String],
  javaSources: List[RawFile],
  projects: List[EnsimeProject]
)

final case class EnsimeProjectId(
  project: String,
  config: String
)

final case class EnsimeProject(
  id: EnsimeProjectId,
  depends: List[EnsimeProjectId],
  sources: Set[RawFile],
  targets: Set[RawFile],
  scalacOptions: List[String],
  javacOptions: List[String],
  libraryJars: List[RawFile],
  librarySources: List[RawFile],
  libraryDocs: List[RawFile]
)

final case class EnsimeServerConfig(
  config: RawFile,
  imports: ImportsConfig,
  shutDownOnDisconnect: Boolean,
  exit: Boolean,
  protocol: String,
  exitAfterIndex: Boolean,
  disableClassMonitoring: Boolean,
  legacy: LegacyConfig
)
final case class LegacyConfig(
  jarurls: Boolean
)
final case class ImportsConfig(
  strategy: String,
  groups: List[String],
  wildcards: Set[String],
  maxIndividualImports: Int,
  collapseExclude: Set[String]
)
