// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.api

import scalaz.deriving

import spray.json.{ JsReader, JsWriter }
import org.ensime.sexp.{ SexpReader, SexpWriter }

@deriving(SexpReader, SexpWriter)
final case class EnsimeConfig(
  rootDir: RawFile,
  cacheDir: RawFile,
  javaHome: RawFile,
  name: String,
  scalaVersion: String,
  javaSources: List[RawFile],
  projects: List[EnsimeProject]
)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter)
final case class EnsimeProjectId(
  project: String,
  config: String
)

@deriving(SexpReader, SexpWriter)
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
  indexBatchSize: Int
)
final case class ImportsConfig(
  locals: Boolean,
  strategy: String,
  groups: List[String],
  wildcards: Set[String],
  maxIndividualImports: Int,
  collapseExclude: Set[String]
)
