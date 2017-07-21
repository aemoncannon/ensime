// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.net.URI
import java.nio.file.Paths

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.pipe
import org.ensime.api._
import org.ensime.indexer.SearchService
import org.ensime.indexer.graph._
import org.ensime.model._
import org.ensime.util.ensimefile.EnsimeFile
import org.ensime.vfs._

// only used for queries by other components
final case class TypeCompletionsReq(prefix: String, maxResults: Int)
final case class FindUsages(fqn: String)
final case class FindHierarchy(fqn: String)

class Indexer(
    index: SearchService,
    implicit val config: EnsimeConfig,
    implicit val vfs: EnsimeVFS
) extends Actor with ActorLogging {

  private def typeResult(hit: FqnSymbol) = TypeSearchResult(
    hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
    LineSourcePositionHelper.fromFqnSymbol(hit)(vfs)
  )

  def oldSearchTypes(query: String, max: Int): List[TypeSearchResult] = {
    // Remove $/$class from the end.
    def strip(fqn: String): String = fqn.replaceAll("\\$(class)*$", "")

    import org.ensime.util.list._

    index.searchClasses(query, max)
      .map {
        case c: ClassDef => c.copy(fqn = strip(c.fqn))
        case f: Field => f.copy(fqn = strip(f.fqn))
        case m: Method => m.copy(fqn = strip(m.fqn))
      }
      .distinctBy(_.fqn)
      .map(typeResult)
  }

  private val typeDecls: Set[DeclaredAs] = Set(DeclaredAs.Class, DeclaredAs.Trait, DeclaredAs.Object)
  def oldSearchSymbols(terms: List[String], max: Int) =
    index.searchClassesMethods(terms, max).flatMap {
      case hit if typeDecls.contains(hit.declAs) => Some(typeResult(hit))
      case hit if hit.declAs == DeclaredAs.Method => Some(MethodSearchResult(
        hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
        LineSourcePositionHelper.fromFqnSymbol(hit)(vfs),
        hit.fqn.split("\\.").init.mkString(".")
      ))
      case _ => None // were never supported
    }

  override def receive = LoggingReceive {
    case ImportSuggestionsReq(file, point, names, maxResults) =>
      val suggestions = names.map(oldSearchTypes(_, maxResults))
      sender ! ImportSuggestions(suggestions)

    case PublicSymbolSearchReq(keywords, maxResults) =>
      val suggestions = oldSearchSymbols(keywords, maxResults)
      sender ! SymbolSearchResults(suggestions)

    case TypeCompletionsReq(query: String, maxResults: Int) =>
      sender ! SymbolSearchResults(oldSearchTypes(query, maxResults))

    case FindUsages(fqn: String) =>
      import context.dispatcher
      val usages = index.findUsages(fqn)
      val response = usages.map { usages =>
        val results: List[LineSourcePosition] = usages.flatMap {
          case (u, line) =>
            val source = u.source
            source.map(s =>
              LineSourcePosition(
                EnsimeFile(Paths.get(new URI(s)).toString),
                line.getOrElse(u.line.getOrElse(0))
              ))
        }(collection.breakOut)
        SourcePositions(results)
      }
      pipe(response) to sender

    case FindHierarchy(fqn: String) =>
      import context.dispatcher
      def toHierarchyInfo(h: Hierarchy): HierarchyInfo = h match {
        case c: ClassDef => ClassInfo(c.fqn, LineSourcePositionHelper.fromFqnSymbol(c))
        case TypeHierarchy(cls, h) => TreeInfo(ClassInfo(cls.fqn, LineSourcePositionHelper.fromFqnSymbol(cls)), h.map(toHierarchyInfo))
      }
      val ancestors = index.getTypeHierarchy(fqn, Hierarchy.Supertypes).map(_.map(toHierarchyInfo))
      val inheritors = index.getTypeHierarchy(fqn, Hierarchy.Subtypes).map(_.map(toHierarchyInfo))
      val symbolTreeInfo = for {
        anc <- ancestors
        inh <- inheritors
      } yield SymbolTreeInfo(anc, inh)
      pipe(symbolTreeInfo) to sender
  }
}
object Indexer {
  def apply(index: SearchService)(implicit config: EnsimeConfig, vfs: EnsimeVFS): Props = Props(classOf[Indexer], index, config, vfs)
}
