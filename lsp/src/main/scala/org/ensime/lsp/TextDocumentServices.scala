// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.io.File
import java.net.URI
import java.nio.charset.Charset

import akka.util.Timeout
import org.ensime.api.{
  CompletionInfo,
  DeclaredAs,
  LineSourcePosition,
  OffsetSourcePosition,
  RawFile,
  SourceFileInfo,
  StructureViewMember,
  SymbolInfo
}
import monix.eval.Task
import org.ensime.util.file._

import scala.meta.lsp._
import scala.meta.jsonrpc.Response
import scribe.Logger

/**
 * Services for operating on text documents such as opening, closing and saving.
 */
final case class TextDocumentServices(
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

              if (uri.getScheme == "file") {
                val f = new File(uri)
                val task =
                  if (f.getAbsolutePath
                        .startsWith(state.cache.rootPath.toString)) {
                    // The file belongs to the ensime cache.  There's no point registering it with ensime
                    Task(log.debug(s"Not adding temporary file $f to Ensime"))
                  } else {
                    log.debug(s"Adding file $f to Ensime")
                    state.project.typecheckFile(f.toPath,
                                                params.textDocument.text)
                  }
                EitherTask.fromTask(task)
              } else {
                EitherTask.fromLeft(
                  TextDocumentServices.UriIsNotAFile(params.textDocument.uri)
                )
              }
            }
        }
    }.raiseError(_.toThrowable)

  /**
   * Called when a client hovers over a position and wants symbol information.
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
                case Some(sig) =>
                  log.info(
                    s"Retrieved $sig for position (${params.position.line}, ${params.position.character})"
                  )
                  Hover(
                    Seq(RawMarkedString("scala", sig)),
                    // TODO: This range should be the start and end of the text to highlight on hover
                    Some(Range(params.position, params.position))
                  )
                case None =>
                  log.info(
                    s"No signature for position (${params.position.line}, ${params.position.character})"
                  )
                  Hover(Seq.empty, None)
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
        val changes = params.contentChanges

        // we assume full text sync
        // for full text sync, we should only have a single change with no range
        EitherTask.fromEither(validateFullTextSync(changes.toList))
      }
      .flatMap[TextDocumentServices.Error, EnsimeProjectWrapper] { _ =>
        getState.map(_.project).leftMap(identity)
      }
      .flatMapTask { project =>
        val change = params.contentChanges.head
        project.typecheckFile(params.textDocument.uri, change.text)
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
            // What does this flag  do?
            .map(CompletionList(false, _))
        }.leftMap(identity)
      }
      .leftMap(_.toResponseError)
      .value

  /** Called when the client requests symbol information */
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

  /** Called when the client requests the location of the definition of the symbol at a position */
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
                  val locationsTask: List[Task[List[Location]]] =
                    declPos.toList.map {
                      case OffsetSourcePosition(sourceFile, offset) =>
                        Task.fromTry(state.cache.getFile(sourceFile)).flatMap {
                          path =>
                            Task.eval {
                              val file = path.toFile
                              val uri  = file.toURI.toString
                              // and what if we can't read the file?
                              val start = TextDocumentServices.offsetToPosition(
                                uri,
                                file
                                  .readString()(
                                    TextDocumentServices.Utf8Charset
                                  )
                                  .toCharArray,
                                offset
                              )
                              val end = start.copy(
                                character = start.character + localName.length()
                              )

                              log.info(
                                s"Found definition at $uri, line: ${start.line}"
                              )
                              List(Location(uri, Range(start, end)))
                            }
                        }
                      case _ => Task(Nil)
                    }
                  val sequencedLocations: Task[List[Location]] =
                    locationsTask.foldLeft(Task(List.empty[Location]))(
                      (prev, cur) =>
                        prev.flatMap { ls =>
                          cur.map(_ ++ ls)
                      }
                    )
                  sequencedLocations
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

  // Move this
  private[lsp] val Utf8Charset: Charset = Charset.forName("UTF-8")

  def apply(ensimeState: Task[Either[Uninitialized.type, EnsimeState]],
            log: Logger): Task[TextDocumentServices] =
    DocumentMap.empty.map(
      TextDocumentServices(ensimeState, _, log)
    )

  /** Map of keywords to [[SymbolKind]] used to convert symbol information */
  private val KeywordToKind = Map(
    "class"   -> SymbolKind.Class,
    "trait"   -> SymbolKind.Interface,
    "type"    -> SymbolKind.Interface,
    "package" -> SymbolKind.Package,
    "def"     -> SymbolKind.Method,
    "val"     -> SymbolKind.Constant,
    "var"     -> SymbolKind.Field
  )

  /**
   * Converts an ensime [[StructureViewMember]] into an LSP [[SymbolInformation]].
   *
   * The member is traversed recursively, with each child being converted.
   */
  def toSymbolInformation(
    uri: String,
    text: String,
    structure: StructureViewMember,
    log: Logger
  ): Seq[SymbolInformation] = {

    // recurse over the members
    def go(member: StructureViewMember,
           outer: Option[String]): Seq[SymbolInformation] = {
      // Why do we default to field?
      val kind = KeywordToKind.getOrElse(member.keyword, SymbolKind.Field)
      val rest = member.members.flatMap(go(_, Some(member.name)))
      val position = member.position match {
        case OffsetSourcePosition(_, offset) =>
          TextDocumentServices.offsetToPosition(uri, text.toArray, offset)
        case LineSourcePosition(_, line) => Position(line, 0)
        case _ =>
          log.error(s"Unknown position for ${member.name}: ${member.position}")
          Position(0, 0)
      }

      SymbolInformation(
        member.name,
        kind,
        Location(uri,
                 Range(position,
                       position.copy(
                         character = position.character + member.name.length
                       ))),
        outer
      ) +: rest
    }

    go(structure, None)
  }

  /**
   * Converts an ensime [[CompletionInfo]] into an LSP [[CompletionItem]]
   */
  def toCompletion(completionInfo: CompletionInfo): CompletionItem = {
    val kind: Option[CompletionItemKind] = completionInfo.typeInfo.map { info =>
      info.declaredAs match {
        case DeclaredAs.Method => CompletionItemKind.Method
        case DeclaredAs.Class  => CompletionItemKind.Class
        case DeclaredAs.Field  => CompletionItemKind.Field
        case DeclaredAs.Interface | DeclaredAs.Trait =>
          CompletionItemKind.Interface
        case DeclaredAs.Object => CompletionItemKind.Module
        // Why do we map everything with a nil type to a value?  Shouldn't this be a None?
        case DeclaredAs.Nil => CompletionItemKind.Value
      }
    }

    CompletionItem(
      label = completionInfo.name,
      kind = kind,
      detail = completionInfo.typeInfo.map(_.fullName)
    )
  }

  def toSourceFileInfo(
    uri: String,
    contents: Option[String] = None
  ): SourceFileInfo = {
    val f = new File(new URI(uri))
    SourceFileInfo(RawFile(f.toPath), contents)
  }

  import scala.concurrent.duration._
  implicit val timeout: Timeout = Timeout(5 seconds)

  def positionToOffset(contents: Array[Char],
                       pos: scala.meta.lsp.Position,
                       uri: String): Int = {
    val line = pos.line
    val col  = pos.character

    var i = 0
    var l = 0

    while (i < contents.length && l < line) {
      contents(i) match {
        case '\r' =>
          l += 1
          if (peek(i + 1, contents) == '\n') i += 1

        case '\n' =>
          l += 1

        case _ =>
      }
      i += 1
    }

    if (l < line)
      throw new IllegalArgumentException(
        s"$uri: Can't find position $pos in contents of only $l lines long."
      )
    if (i + col < contents.length)
      i + col
    else
      throw new IllegalArgumentException(
        s"$uri: Invalid column. Position $pos in line '${contents.slice(i, contents.length).mkString}'"
      )
  }

  /**
   * Return the corresponding position in this text document as 0-based line and column.
   */
  def offsetToPosition(uri: String,
                       contents: Array[Char],
                       offset: Int): scala.meta.lsp.Position = {
    if (offset >= contents.length)
      throw new IndexOutOfBoundsException(
        s"$uri: asked position at offset $offset, but contents is only ${contents.length} characters long."
      )

    var i    = 0
    var line = 0
    var col  = 0

    while (i < offset) {
      contents(i) match {
        case '\r' =>
          line += 1
          col = 0
          if (peek(i + 1, contents) == '\n') i += 1

        case '\n' =>
          line += 1
          col = 0

        case _ =>
          col += 1
      }
      i += 1
    }

    scala.meta.lsp.Position(line, col)
  }

  private[this] def peek(idx: Int, contents: Array[Char]): Int =
    if (idx < contents.length) contents(idx).toInt else -1

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
