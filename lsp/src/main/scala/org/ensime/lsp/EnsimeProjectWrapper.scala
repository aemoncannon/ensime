// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.io.File
import java.nio.file.Path
import java.net.URI

import akka.actor.ActorRef
import akka.util.Timeout
import akka.pattern.ask
import org.ensime.api._
import org.ensime.core.{ DocSig, DocSigPair }
import monix.eval.Task
import scala.meta.lsp.{ CompletionItem, Position, SymbolInformation }

/**
 * Wraps requests to the Ensime project actor in [[Task]].  These can then be
 * lifted into a larger [[Task]] sequence, and ultimately into an LSP service.
 */
final case class EnsimeProjectWrapper(project: ActorRef) {

  /**
   * Tell the ensime project actor to remove a document from its internal state.
   * This is called when the user closes a document.
   */
  def removeFile(uri: String): Task[Unit] =
    Task.eval(new File(new URI(uri))).map { file =>
      project ! RemoveFileReq(file)
    }

  /**
   * Tell the ensime project actor to typecheck a document.  This is called
   * whenever a document is modified.
   *
   * @param uri  The uri corresponding to the document's location
   * @param text The text content of the document
   */
  def typecheckFile(uri: String, text: String): Task[Unit] =
    LspToEnsimeAdapter.toSourceFileInfo(uri, text).map { sourceFileInfo =>
      project ! TypecheckFileReq(sourceFileInfo)
    }

  def typecheckFile(path: Path, text: String): Task[Unit] =
    Task(project ! TypecheckFileReq(SourceFileInfo(RawFile(path), Some(text))))

  /**
   * Ask the ensime project actor for the signature of the thing at the
   * specified position.  This is called whenever a user hovers over a thing
   * with the intent to view its signature.
   *
   * @param uri       The uri corresponding to the document containing the thing
   * @param text      The text content of the document containing the thing.
   * @param position  The position of the thing within the text
   */
  def getSignatureAtPoint(uri: String, text: String, position: Position)(
    implicit T: Timeout
  ): Task[Option[Signature]] =
    for {
      sourceFileInfo <- LspToEnsimeAdapter.toSourceFileInfo(uri, text)
      offset <- fromEither(
                 LspToEnsimeAdapter
                   .positionToOffset(uri, text, position)
                   .left
                   .map(_.toIllegalArgumentException)
               )
      offsetRange = OffsetRange(offset)
      result <- Task.fromFuture(
                 project ? DocUriAtPointReq(Right(sourceFileInfo), offsetRange)
               )
    } yield
      result match {
        case pair @ Some(DocSigPair(DocSig(_, scalaSig), DocSig(_, javaSig))) =>
          scalaSig.map(Signature.Scala).orElse(javaSig.map(Signature.Java))
        case None => None
      }

  /**
   * Ask the ensime project actor for code completions of the thing at the
   * specified position.
   *
   * @param uri       The uri corresponding to the document containing the thing
   * @param text      The text content of the document containing the thing.
   * @param position  The position of the thing within the text
   * @return          A list of possible completions of the thing.  At most 100
   *                  results are returned. Completions are case-insensitive.
   */
  def getCompletions(uri: String, text: String, position: Position)(
    implicit T: Timeout
  ): Task[List[CompletionItem]] =
    for {
      sourceFileInfo <- LspToEnsimeAdapter.toSourceFileInfo(uri, text)
      offset <- fromEither(
                 LspToEnsimeAdapter
                   .positionToOffset(uri, text, position)
                   .left
                   .map(_.toIllegalArgumentException)
               )
      result <- Task.fromFuture(
                 project ? CompletionsReq(
                   sourceFileInfo,
                   offset,
                   maxResults = 100,
                   caseSens = false,
                   // I'm not sure what this does - the value of "reload" is
                   // unimportant to the project (see [[Analyzer.allTheThings]])
                   reload = false
                 )
               )
    } yield
      result match {
        case CompletionInfoList(prefix, completions) =>
          completions
            .map(EnsimeToLspAdapter.completionInfoToCompletionItem)
      }

  /**
   * Asks the ensime project actor for the structure view of a document.
   * The structure view members are converted into a flat list of
   * [[SymbolInformation]].
   *
   * @param uri  The uri corresponding to the document's location
   * @param text The text corresponding to the document
   * @return     A flat list of all [[SymbolInformation]] in the document's
   *             structure.
   */
  def getSymbolInformation(uri: String, text: String)(
    implicit T: Timeout
  ): Task[List[SymbolInformation]] =
    LspToEnsimeAdapter
      .toSourceFileInfo(uri, text)
      .flatMap { sourceFileInfo =>
        Task.fromFuture(project ? StructureViewReq(sourceFileInfo))
      }
      .map {
        case StructureView(members) =>
          members.flatMap(
            EnsimeToLspAdapter
              .structureViewMemberToSymbolInformation(uri, text, _)
          )
      }

  /**
   * Gets the Ensime [[SymbolInfo]] of a thing at a point.
   *
   * @param uri       The uri corresponding to the document containing the thing
   * @param text      The text content of the document containing the thing.
   * @param position  The position of the thing within the text
   * @return          The [[SymbolInfo]] of the thing.
   */
  def getSymbolAtPoint(uri: String, text: String, position: Position)(
    implicit T: Timeout
  ): Task[SymbolInfo] =
    for {
      sourceFileInfo <- LspToEnsimeAdapter.toSourceFileInfo(uri, text)
      offset <- fromEither(
                 LspToEnsimeAdapter
                   .positionToOffset(uri, text, position)
                   .left
                   .map(_.toIllegalArgumentException)
               )
      result <- Task.fromFuture(
                 project ? SymbolAtPointReq(Right(sourceFileInfo), offset)
               )
    } yield result.asInstanceOf[SymbolInfo]

  /**
   * Convers an [[Either]] into a [[Task]].  This should be included in the
   * most recent version of Monix
   */
  private def fromEither[A](either: Either[Throwable, A]): Task[A] =
    either match {
      case Left(err) => Task.raiseError(err)
      case Right(a)  => Task(a)
    }
}
