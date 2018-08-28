// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.io.File
import java.net.URI
import java.nio.charset.Charset

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ensime.api.{RemoveFileReq, TypecheckFileReq, SourceFileInfo, RawFile, CompletionsReq, CompletionInfoList, CompletionInfo, DeclaredAs, DocUriAtPointReq, OffsetRange, StructureViewReq, StructureView, StructureViewMember, OffsetSourcePosition, LineSourcePosition, SymbolAtPointReq, SymbolInfo}
import monix.eval.{Task, MVar}
import org.ensime.util.file._
import org.ensime.util.path._

import scala.meta.lsp._
import scala.meta.jsonrpc.Response
import scribe.Logger

import org.ensime.core.{DocSigPair, DocSig}

final case class DocumentMap(state: MVar[Map[String, TextDocumentItem]]) {

  def get(uri: String): Task[Option[TextDocumentItem]] = state.read.map(_.get(uri))

  def put(uri: String, item: TextDocumentItem): Task[Unit] = modify(_ + (uri -> item))

  def remove(uri: String): Task[Unit] = modify(_ - uri)

  def contains(uri: String): Task[Boolean] = state.read.map(_.contains(uri))

  private def modify(f: Map[String, TextDocumentItem] => Map[String, TextDocumentItem]): Task[Unit] = for {
    docs <- state.take
    _    <- state.put(f(docs))
  } yield ()
}

object DocumentMap {
  def empty: Task[DocumentMap] = Task(MVar(Map.empty[String, TextDocumentItem])).map(DocumentMap(_))
}

final case class OptionalRef[A](state: MVar[Option[A]]) {

  def put(ref: A): Task[Unit] = for {
    _ <- state.take
    _ <- state.put(Some(ref))
  } yield ()

  def get: Task[Either[Uninitialized.type, A]] =
    state.read.map(_.toRight(Uninitialized))
}

object OptionalRef {
  def empty[A]: Task[OptionalRef[A]] = Task(MVar(Option.empty[A])).map(OptionalRef(_))
}

object Uninitialized

final case class TextDocumentServices(initialState: OptionalRef[(EnsimeProjectWrapper, EnsimeCache)], documentMap: DocumentMap, log: Logger) {

  /**
    * Called when the client closes a text document.
    *
    * The document uri is removed from the documentManager and the file is removed from Ensime.
    */
  def didClose(params: DidCloseTextDocumentParams): Task[Unit] = withEnsime { ensime =>

    Task(log.info(s"Removing ${params.textDocument.uri} from Ensime.")).flatMap { _ =>
      documentMap.contains(params.textDocument.uri).map { contains =>
        if (contains) {
          ensime.removeFile(params.textDocument.uri)
        } else {
          log.error(s"Attempted to close document ${params.textDocument.uri}, but it wasn't in an open state")
        }
      }.flatMap { _ =>
        documentMap.remove(params.textDocument.uri)
      }
    }
  }.map(_ => ())

  /**
    * Called when the client opens a text document.
    *
    * The document uri is registered as being open in the documentManager and the file is sent to ensime for typechecking.
    */
  def didOpen(params: DidOpenTextDocumentParams): Task[Unit] = withInitialState { case (ensime, ensimeCache) =>

    Task(log.info(s"Got didOpen (${params.textDocument.uri})")).flatMap { _ =>
      // Store the file in the document map
      documentMap.put(params.textDocument.uri, params.textDocument).map { _ =>
        val uri = new URI(params.textDocument.uri)
        if (uri.getScheme == "file") {
          val f = new File(uri)
          if (f.getAbsolutePath.startsWith(ensimeCache.path)) {
            // The file belongs to the ensime cache.  There's no point registering it with ensime
            log.debug(s"Not adding temporary file $f to Ensime")
          } else {
            log.debug(s"Adding file $f to Ensime")
            ensime.typecheckFile(f.toPath, params.textDocument.text)
          }
        } else {
          log.info(s"Non-file URI in openTextDocument: ${params.textDocument.uri}")
        }
      }
    }
  }.map(_ => ())


  def hover(params: TextDocumentPositionParams)(implicit T: Timeout): Task[Either[Response.Error, Hover]] = withEnsime { ensime =>

    Task(log.info(s"Got hover request at (${params.position.line}, ${params.position.character}).")).flatMap { _ =>
      documentMap.get(params.textDocument.uri).flatMap(_.map { doc =>

        val docUriAtPoint = ensime.getDocUriAtPoint(params.textDocument.uri, doc.text, params.position)
        docUriAtPoint.map {
          case Some(sig) =>
            log.info(s"Retrieved $sig for position (${params.position.line}, ${params.position.character})")
            Hover(Seq(RawMarkedString("scala", sig)),
              // TODO: This range should be the start and end of the text to highlight on hover
              Some(Range(params.position, params.position)))
          case None =>
            log.info(s"No signature for position (${params.position.line}, ${params.position.character})")
            Hover(Seq.empty, None)
        }
      }.getOrElse {
        Task {
          log.info("Document is not in an opened state")
          Hover(Seq.empty, None)
        }
      })
    }
  }

  def didSave(params: DidSaveTextDocumentParams): Task[Unit] = Task {
    log.info("Document was saved")
  }

  def willSave(params: WillSaveTextDocumentParams): Task[Unit] = Task {
    log.info("Document will be saved")
  }

  def willSaveWaitUntil(params: WillSaveTextDocumentParams): Task[Either[Response.Error,List[TextEdit]]] = Task {
    log.info("Document will save wait until")
    Right(Nil)
  }

  def didChange(params: DidChangeTextDocumentParams): Task[Unit] = {
    log.info(s"Document did change (${params.contentChanges})")
    val changes = params.contentChanges
    // we assume full text sync
    // for full text sync, we should only have a single change with no range
    val assertCorrectChanges = Task {
      assert(changes.size == 1)
      val change = changes.head
      assert(change.range.isEmpty)
      assert(change.rangeLength.isEmpty)
    }

    assertCorrectChanges.flatMap { _ => withEnsime { ensime =>
      // TODO: Don't we need to update the document manager here?
      val change = changes.head
      ensime.typecheckFile(params.textDocument.uri, change.text)
    }}.map(_ => ())
  }

  def completion(params: TextDocumentPositionParams)(implicit T: Timeout): Task[Either[Response.Error, CompletionList]] = {
    withEnsime { ensime =>
      documentMap.get(params.textDocument.uri).flatMap { docOpt =>
        docOpt.map { doc =>
          ensime.getCompletions(params.textDocument.uri, doc.text, params.position).map(CompletionList(false, _))
        }.getOrElse { Task {
          log.error(s"Document ${params.textDocument.uri} is not in an open state")
          (CompletionList(false, Nil))
        }
        }
      }
    }
  }

  def documentSymbol(params: DocumentSymbolParams)(implicit T: Timeout): Task[Either[Response.Error, List[SymbolInformation]]] = {
    withEnsime { ensime =>
      documentMap.get(params.textDocument.uri).flatMap { docOpt =>
        docOpt.map { doc =>
          log.info(s"Document Symbols request for ${params.textDocument.uri}")
          ensime.getSymbolInformation(params.textDocument.uri, doc.text)
        }.getOrElse {
          Task {
            log.error(s"Document ${params.textDocument.uri} is not in an open state")
            Nil
          }
        }
      }
    }
  }

  def definition(params: TextDocumentPositionParams)(implicit T: Timeout): Task[Either[Response.Error, List[Location]]] = withInitialState {
    case (ensime, ensimeCache) =>
    log.info(s"Got goto definition request at (${params.position.line}, ${params.position.character}).")
    documentMap.get(params.textDocument.uri).flatMap { docOpt =>
      docOpt.map { doc =>
        ensime.getSymbolAtPoint(params.textDocument.uri, doc.text, params.position)
        .map {
          case SymbolInfo(name, localName, declPos, typeInfo) =>
            declPos.toList.flatMap {
              // Why would this be an ensime file? The file containing the symbol, and its offset
              case OffsetSourcePosition(ensimeFile, offset) =>
                  ensimeCache
                    .getFile(ensimeFile)
                    .map(path => {
                      val file = path.toFile
                      val uri  = file.toURI.toString
                      // and what if we can't read the file?
                      val start = TextDocumentServices.offsetToPosition(uri, file.readString()(TextDocumentServices.Utf8Charset).toCharArray, offset)
                      val end = start.copy(character = start.character + localName.length())

                      log.info(s"Found definition at $uri, line: ${start.line}")
                      List(Location(uri, Range(start, end)))
                    })
                  .recover {
                    case e =>
                      log.error(s"Couldn't retrieve hyperlink target file $e")
                      Nil
                  }
                  .get
              case _ => Nil
            }
        }
      }.getOrElse(Task(List.empty[Location]))
    }
  }

  def references(params: ReferenceParams): Task[List[Location]] = ???

  def codeAction(params: CodeActionParams): Task[List[Command]] = ???

  def documentHighlight(params: TextDocumentPositionParams): Task[List[DocumentHighlight]] = ???

  def formatting(params: DocumentFormattingParams): Task[List[TextEdit]] = ???

  def rename(params: RenameParams): Task[WorkspaceEdit] = ???

  def signatureHelp(params: TextDocumentPositionParams): Task[SignatureHelp] = ???


  private def withEnsime[A](f: EnsimeProjectWrapper => Task[A]): Task[Either[Response.Error, A]] =
    initialState.get.flatMap {
      case Left(_) => Task(Left(Response.internalError("Ensime not initialized yet")))
      case Right((ref, _)) => f(ref).map(Right(_))
    }

  private def withInitialState[A](f: ((EnsimeProjectWrapper, EnsimeCache)) => Task[A]): Task[Either[Response.Error, A]] =
    initialState.get.flatMap {
      case Left(_) => Task(Left(Response.internalError("Ensime not initialized yet")))
      case Right(state) => f(state).map(Right(_))
    }

}

object TextDocumentServices {

  // Move this
  private[lsp] val Utf8Charset: Charset  = Charset.forName("UTF-8")

  def apply(initialState: OptionalRef[(EnsimeProjectWrapper, EnsimeCache)], log: Logger): Task[TextDocumentServices] =
    DocumentMap.empty.map(TextDocumentServices(initialState, _, log))

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
    def go(member: StructureViewMember, outer: Option[String]): Seq[SymbolInformation] = {
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
        case DeclaredAs.Interface | DeclaredAs.Trait => CompletionItemKind.Interface
        case DeclaredAs.Object => CompletionItemKind.Module
        // Why do we map everything with a nil type to a value?  Shouldn't this be a None?
        case DeclaredAs.Nil    => CompletionItemKind.Value
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
  implicit val timeout: Timeout     = Timeout(5 seconds)

  def positionToOffset(contents: Array[Char], pos: scala.meta.lsp.Position, uri: String): Int = {
    val line = pos.line
    val col = pos.character

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
  def offsetToPosition(uri: String, contents: Array[Char], offset: Int): scala.meta.lsp.Position = {
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
}
