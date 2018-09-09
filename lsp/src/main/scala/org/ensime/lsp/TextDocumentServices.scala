// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.io.File
import java.net.URI

import akka.util.Timeout
import org.ensime.api.{ OffsetSourcePosition, SymbolInfo }
import monix.eval.Task
import org.ensime.util.file._

import scala.meta.lsp._
import scala.meta.jsonrpc.Response
import scribe.Logger

/**
  * Services for operating on text documents such as opening, closing and saving.
  *
  * @param ensimeState  The Ensime actors.  This is uninitialized until an initialize request is received from the LSP client
  * @param documentMap  A cache of all open documents
 */
final class TextDocumentServices(
  ensimeState: Task[Either[Uninitialized.type, EnsimeState]],
  documentMap: DocumentMap,
  log: Logger
) {

  /**
   * Called when the client closes a text document.
   *
   * The document is removed from the [[DocumentMap]] and the file is removed from Ensime.
   */
  def didClose(params: DidCloseTextDocumentParams): Task[Unit] =
    getState
      .map(_.project)
      .flatMap { project =>
        EitherTask
          .fromTask(
            Task(log.info(s"Removing ${params.textDocument.uri} from Ensime."))
          )
          .flatMap { _ =>
            EitherTask
              .fromTask[TextDocumentServices.Error, Boolean](
                documentMap.contains(params.textDocument.uri)
              )
              .flatMap[TextDocumentServices.Error, Unit] { exists =>
                if (exists) {
                  EitherTask.fromTask(
                    project.removeFile(params.textDocument.uri)
                  )
                } else {
                  EitherTask.fromLeft(
                    TextDocumentServices
                      .DocumentNotOpen(params.textDocument.uri)
                  )
                }
              }
              .flatMap { _ =>
                EitherTask.fromTask[TextDocumentServices.Error, Unit](
                  documentMap.remove(params.textDocument.uri)
                )
              }
          }
      }
      .raiseError(_.toThrowable)

  /**
   * Called when the client opens a text document.
   *
   * The document is stored in the [[DocumentMap]] and the file is sent to ensime for typechecking.
   */
  def didOpen(params: DidOpenTextDocumentParams): Task[Unit] =
    getState.flatMap { state =>
      EitherTask
        .fromTask(Task(log.info(s"Got didOpen (${params.textDocument.uri})")))
        .flatMap { _ =>
          EitherTask
            .fromTask[TextDocumentServices.Error, Unit](
              documentMap.put(params.textDocument.uri, params.textDocument)
            )
            .flatMap[TextDocumentServices.Error, Unit] { _ =>
              val uri = new URI(params.textDocument.uri)

              if (uri.getScheme == TextDocumentServices.fileScheme) {
                val f = new File(uri)
                val task =
                  if (state.cache.contains(f)) {
                    // The file belongs to the ensime cache.  There's no point registering it with ensime
                    Task(log.debug(s"Not adding temporary file $f to Ensime"))
                  } else {
                    log.debug(s"Adding file $f to Ensime")
                    state.project.typecheckFile(f.toPath,
                                                params.textDocument.text)
                  }
                EitherTask.fromTask(task)
              } else {
                // Whatever the user has client has opened isn't a file. This probably isn't particularly likely, but we were checking for it before.
                EitherTask.fromLeft(
                  TextDocumentServices.UriIsNotAFile(params.textDocument.uri)
                )
              }
            }
        }
    }.raiseError(_.toThrowable)

  /**
   * Called when a client hovers over a position and wants symbol information.  At most one signature is returned
   */
  def hover(params: TextDocumentPositionParams)(
    implicit T: Timeout
  ): Task[Either[Response.Error, Hover]] =
    getState
      .map(_.project)
      .flatMap { project =>
        EitherTask
          .fromTask[TextDocumentServices.Error, Unit](
            Task(
              log.info(
                s"Got hover request at (${params.position.line}, ${params.position.character})."
              )
            )
          )
          .flatMap { _ =>
            getOpenDocument(params.textDocument.uri).flatMap { doc =>
              val docUriAtPoint =
                project.getDocUriAtPoint(params.textDocument.uri,
                                         doc.text,
                                         params.position)
              val hover = docUriAtPoint.map {
                case Signature(scala, java) =>
                  log.info(
                    s"Retrieved $scala and $java for position (${params.position.line}, ${params.position.character})"
                  )

                  // Return the scala or java signature
                  val signature = scala.map(RawMarkedString("scala", _)).orElse(java.map(RawMarkedString("java", _)))

                    // The range should be the start and end of the text to highlight on hover.  I think we may be able to get this information by calling [[getSymbolAtPoint]], but for now we return a range of zero length around the position.
                  val range = Range(params.position, params.position)
                  signature.map(s => Hover(Seq(s), Some(range)))
                  .getOrElse(Hover(Seq.empty, None))
              }
              EitherTask.fromTask[TextDocumentServices.Error, Hover](hover)
            }
          }
      }
      .leftMap(_.toResponseError)
      .value

  /** Called when the client has saved a document */
  def didSave(params: DidSaveTextDocumentParams): Task[Unit] = Task {
    log.info("Document was saved")
  }

  /** Called when the client is about to save a document */
  def willSave(params: WillSaveTextDocumentParams): Task[Unit] = Task {
    log.info("Document will be saved")
  }

  // Not sure what this does
  def willSaveWaitUntil(
    params: WillSaveTextDocumentParams
  ): Task[Either[Response.Error, List[TextEdit]]] = Task {
    log.info("Document will save wait until")
    Right(Nil)
  }

  /**
   * Called when a document was edited
   *
   * Ensime cannot process incremental edits (it requires full text sync).  We validate that full text sync is taking place before sending the changes to the ensime project to typecheck the file.
   */
  def didChange(params: DidChangeTextDocumentParams): Task[Unit] =
    EitherTask
      .fromTask[TextDocumentServices.Error, Unit](
        Task(log.info(s"Document did change (${params.contentChanges})"))
      )
      .flatMap[TextDocumentServices.Error, Unit] { _ =>
        // validate that the client is sending complete documents as changes (i.e. not incremental changes).  We've configured it to send full documents on initialization.  @see [[LifecycleServices.initialize]]
        EitherTask.fromEither(
          validateFullTextSync(params.contentChanges.toList)
        )
      }
      .flatMap[TextDocumentServices.Error, EnsimeProjectWrapper] { _ =>
        getState.map(_.project).leftMap(identity)
      }
      .flatMapTask { project =>
        project.typecheckFile(params.textDocument.uri,
                              params.contentChanges.head.text)
      }
      .raiseError(_.toThrowable)

  /** Called when the client requests symbol completion information
   */
  def completion(
    params: TextDocumentPositionParams
  )(implicit T: Timeout): Task[Either[Response.Error, CompletionList]] =
    getState
      .map(_.project)
      .flatMap[TextDocumentServices.Error, CompletionList] { project =>
        getOpenDocument(params.textDocument.uri).flatMapTask { doc =>
          project
            .getCompletions(params.textDocument.uri, doc.text, params.position)
            // We return practically all completions, so the completion list is complete
            .map(CompletionList(isIncomplete = false, _))
        }.leftMap(identity)
      }
      .leftMap(_.toResponseError)
      .value

  /** Called when the client requests symbol information of a document.  The symbol information is a structure view of an entire document. */
  def documentSymbol(params: DocumentSymbolParams)(
    implicit T: Timeout
  ): Task[Either[Response.Error, List[SymbolInformation]]] =
    getState
      .map(_.project)
      .flatMap[TextDocumentServices.Error, List[SymbolInformation]] { project =>
        getOpenDocument(params.textDocument.uri).flatMapTask { doc =>
          project.getSymbolInformation(params.textDocument.uri, doc.text)
        }.leftMap(identity)
      }
      .leftMap(_.toResponseError)
      .value

  /**
   * Called when the client requests the location of a symbol's definition
   *
   * @return A list of at most one location
   * */
  def definition(params: TextDocumentPositionParams)(
    implicit T: Timeout
  ): Task[Either[Response.Error, List[Location]]] =
    getState.flatMap { state =>
      EitherTask
        .fromTask[TextDocumentServices.Error, Unit](
          Task(
            log.info(
              s"Got goto definition request at (${params.position.line}, ${params.position.character})."
            )
          )
        )
        .flatMap[TextDocumentServices.Error, List[Location]] { _ =>
          getOpenDocument(params.textDocument.uri).flatMapTask { doc =>
            state.project
              .getSymbolAtPoint(params.textDocument.uri,
                                doc.text,
                                params.position)
              .flatMap {
                case SymbolInfo(name, localName, declPos, typeInfo) =>
                  val offsetSourcePosition: Option[OffsetSourcePosition] =
                    declPos.collect {
                      case o: OffsetSourcePosition =>
                        // We only care about these. [[EmptySourcePosition]] implies that Ensime can't find the position
                        // [[getSymbolAtPoint]] should never return a [[LineSourcePosition]] (those are only used when finding usages)
                        o
                    }
                  val locationOption: Option[Task[Location]] =
                    offsetSourcePosition.map {
                      case OffsetSourcePosition(sourceFile, offset) =>
                        state.cache.path(sourceFile).flatMap { path =>
                          EnsimeCache.fileText(path).map { text =>
                            val uri = path.toFile.toURI.toString
                            val startPositionEither = EnsimeToLspAdapter
                              .offsetToPosition(
                                uri,
                                text,
                                offset
                              )

                            // We can find the source document, but cannot convert the symbol's offset to a valid position.  Return a position at the start of the document so that the user knows roughly where the symbol is.
                            val start = startPositionEither.toOption
                              .getOrElse(Position(0, 0))
                            val range =
                              TextDocumentServices.rangeFrom(start,
                                                             localName.length)
                            log.info(
                              s"Found definition at $uri, line: ${start.line}"
                            )
                            Location(uri, range)
                          }
                        }
                    }
                  // Return a list of at most one location
                  locationOption
                    .map(_.map(List(_)))
                    .getOrElse(Task(List.empty[Location]))
              }
          }.leftMap(identity)
        }
    }.leftMap(_.toResponseError).value

  def references(params: ReferenceParams): Task[List[Location]] = ???

  def codeAction(params: CodeActionParams): Task[List[Command]] = ???

  def documentHighlight(
    params: TextDocumentPositionParams
  ): Task[List[DocumentHighlight]] = ???

  def formatting(params: DocumentFormattingParams): Task[List[TextEdit]] = ???

  def rename(params: RenameParams): Task[WorkspaceEdit] = ???

  def signatureHelp(params: TextDocumentPositionParams): Task[SignatureHelp] =
    ???

  private def getState
    : EitherTask[TextDocumentServices.EnsimeSystemUninitialized.type,
                 EnsimeState] =
    EitherTask(ensimeState).leftMap(
      _ => TextDocumentServices.EnsimeSystemUninitialized
    )

  private def getOpenDocument(
    uri: String
  ): EitherTask[TextDocumentServices.DocumentNotOpen, TextDocumentItem] =
    EitherTask(
      documentMap
        .get(uri)
        .map(_.toRight(TextDocumentServices.DocumentNotOpen(uri)))
    )

  private def validateFullTextSync(
    changes: List[TextDocumentContentChangeEvent]
  ): Either[TextDocumentServices.TextDocumentSyncConfigError, Unit] =
    if (changes.size != 1) {
      Left(TextDocumentServices.MultipleChangesDetected(changes))
    } else {
      val change = changes.head
      if (!change.range.isEmpty) {
        Left(TextDocumentServices.ChangeRangeNonEmpty(changes))
      } else if (!change.rangeLength.isEmpty) {
        Left(TextDocumentServices.ChangeRangeLengthNonEmpty(changes))
      } else {
        Right(())
      }
    }

}

object TextDocumentServices {

  private [lsp] val fileScheme: String = "file"

  def apply(ensimeState: Task[Either[Uninitialized.type, EnsimeState]],
            log: Logger): Task[TextDocumentServices] =
    DocumentMap.empty.map(
      new TextDocumentServices(ensimeState, _, log)
    )

  /**
   * Creates a [[Range]] from a start [[Position]] and offset.
   *
   * The end position is on the same line as the start, but offset by [[offset]] characters.
   */
  private[lsp] def rangeFrom(start: Position, offset: Int): Range = {
    val end = start.copy(character = start.character + offset)
    Range(start, end)
  }

  sealed trait Error {
    def message: String = this match {
      case EnsimeSystemUninitialized => Uninitialized.message
      case DocumentNotOpen(uri)      => s"Document $uri not in an open state"
      case UriIsNotAFile(uri)        => s"Document $uri is not a file"
      case MultipleChangesDetected(changes) =>
        s"Incorrect text sync: ${changes.length} changes detected.  Ensime requires full text sync"
      case ChangeRangeNonEmpty(changes) =>
        s"Change range non-empty: ${changes}.  Ensime requires full text sync"
      case ChangeRangeLengthNonEmpty(changes) =>
        s"Change range length non-empty: ${changes}.  Ensime requires full text sync"
    }
    def toThrowable: Throwable          = new IllegalArgumentException(message)
    def toResponseError: Response.Error = Response.internalError(message)
  }

  case object EnsimeSystemUninitialized         extends Error
  final case class DocumentNotOpen(uri: String) extends Error
  final case class UriIsNotAFile(uri: String)   extends Error

  sealed trait TextDocumentSyncConfigError extends Error
  final case class MultipleChangesDetected(
    changes: List[TextDocumentContentChangeEvent]
  ) extends TextDocumentSyncConfigError
  final case class ChangeRangeNonEmpty(
    changes: List[TextDocumentContentChangeEvent]
  ) extends TextDocumentSyncConfigError
  final case class ChangeRangeLengthNonEmpty(
    changes: List[TextDocumentContentChangeEvent]
  ) extends TextDocumentSyncConfigError
}
