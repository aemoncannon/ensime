// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import scala.concurrent._

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.ensime.api._
import org.ensime.vfs._

final case class FqnSymbol(
  val fqn: String,
  val line: Option[Int],
  val source: Option[String],
  val declAs: DeclaredAs
) {
  // used in the tests (/me crying in a corner, again)
  def toSearchResult: String = s"$declAs $fqn"
}

final case class UsageLocation(file: Option[String], line: Option[Int])

class SearchService(
  config: EnsimeConfig,
  resolver: SourceResolver
)(
  implicit
  serverConfig: EnsimeServerConfig,
  val vfs: EnsimeVFS
) extends FileChangeListener
    with SLF4JLogging {

  // these methods are a mess... some are async, some are blocking, all are
  // performing side-effects (/me whimpers in a corner)

  // used by Project on initialisation
  def refresh(): Future[(Int, Int)] = ???

  // used in tests
  def refreshResolver(): Unit = ???

  // used by Index actor
  def searchClasses(query: String, max: Int): List[FqnSymbol]              = ???
  def searchClassesMethods(terms: List[String], max: Int): List[FqnSymbol] = ???

  def getHierarchy(fqn: String): Future[List[HierarchyInfo]] = ???

  def findUsageLocations(fqn: String): Future[Iterable[UsageLocation]] = ???

  // used by ModelBuilders (and Java equivalent)
  def findUnique(fqn: String): Option[FqnSymbol] = ???

  // used by Presentation Compiler for refactorings. would be better to use
  // SymbolOps from semanticdb-scalac to get the semanticdb name straight from
  // the scalac symbol.
  def findUsages(fqn: String): Future[Iterable[FqnSymbol]] = ???

  // callbacks to let us know that binaries have changed
  def fileChanged(f: FileObject): Unit = ???
  def fileRemoved(f: FileObject): Unit = fileChanged(f)
  def fileAdded(f: FileObject): Unit   = fileChanged(f)

  def shutdown(): Future[Unit] = ???
}
