// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import java.io.File
import java.net.URI

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ensime.api._
import org.ensime.lsp.core.{TextDocumentManager, TextDocument}
import org.ensime.util.file._
import monix.eval.Task

import scala.meta.lsp.{DidOpenTextDocumentParams, DidCloseTextDocumentParams, TextDocumentPositionParams, DidSaveTextDocumentParams, DidChangeTextDocumentParams, Hover, RawMarkedString, Range, CompletionList, CompletionItem, CompletionItemKind, DocumentSymbolParams, SymbolInformation, SymbolKind, Position, Location}
import scribe.Logger

import org.ensime.core.{DocSigPair, DocSig}

object TextDocumentServices {

  /**
    * Called when the client opens a text document.
    *
    * The document uri is registered as being open in the documentManager and the file is sent to ensime for typechecking.
    */
  def didOpen(params: DidOpenTextDocumentParams, ensimeActor: ActorRef, fileStore: TempFileStore, documentManager: TextDocumentManager, log: Logger): Task[Unit] = Task {

    // Store the file in the document manager
    documentManager.onOpenTextDocument(params.textDocument)
    val uri = new URI(params.textDocument.uri)
    if (uri.getScheme == "file") {
      val f = new File(uri)
      if (f.getAbsolutePath.startsWith(fileStore.path)) {
        // The file belongs to the ensime cache.  There's no point registering it with ensime
        log.debug(s"Not adding temporary file $f to Ensime")
      } else {
        log.debug(s"Adding file $f to Ensime")
        ensimeActor ! TypecheckFileReq(
          SourceFileInfo(RawFile(f.toPath), Some(params.textDocument.text))
        )
      }
    } else {
      log.info(s"Non-file URI in openTextDocument: ${params.textDocument.uri}")
    }
  }

  /**
    * Called when the client closes a text document.
    *
    * The document uri is removed from the documentManager and the file is removed from Ensime.
    */
  def didClose(params: DidCloseTextDocumentParams, ensimeActor: ActorRef, documentManager: TextDocumentManager, log: Logger): Task[Unit] = Task {

    log.debug(s"Removing ${params.textDocument.uri} from Ensime.")

    // Get the file from the document manager
    val doc = documentManager.documentForUri(params.textDocument.uri)
    // Remove the file from ensime
    doc.foreach(d => ensimeActor ! RemoveFileReq(new File(URI.create(params.textDocument.uri))))

    // Remove the file from the document manager
    documentManager.onCloseTextDocument(params.textDocument)
  }

  /**
    * Called when the client hovers over a point in a text document.
    *
    * Calculates a signature
    */
  def hover(params: TextDocumentPositionParams, ensimeActor: ActorRef, documentManager: TextDocumentManager, log: Logger)(implicit T: Timeout): Task[Hover] = {

    log.info(s"Got hover request at (${params.position.line}, ${params.position.character}).")

    documentManager.documentForUri(params.textDocument.uri).map { doc =>

      val docUriAtPoint = Task.fromFuture(ensimeActor ? DocUriAtPointReq(
        Right(
          EnsimeLanguageServer.toSourceFileInfo(
            params.textDocument.uri,
            Some(new String(doc.text.toArray))
          )
        ),
        OffsetRange(org.ensime.lsp.core.TextDocument.positionToOffset(doc.text.toArray, params.position, params.textDocument.uri))
      ))

      docUriAtPoint.map {
        case pair@Some(DocSigPair(DocSig(_, scalaSig), DocSig(_, javaSig))) =>
          val sig = scalaSig.orElse(javaSig).getOrElse("")
          log.info(s"Retrieved $pair for position (${params.position.line}, ${params.position.character})")
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
    }
  }

  def didSave(params: DidSaveTextDocumentParams, log: Logger): Task[Unit] = Task {
    log.info("Document was saved")
  }

  def didChange(params: DidChangeTextDocumentParams, ensimeActor: ActorRef, log: Logger): Task[Unit] = Task {

    val changes = params.contentChanges
    // we assume full text sync
    assert(changes.size == 1)
    val change = changes.head
    assert(change.range.isEmpty)
    assert(change.rangeLength.isEmpty)

    ensimeActor ! TypecheckFileReq(
      EnsimeLanguageServer.toSourceFileInfo(params.textDocument.uri, Some(change.text))
    )
  }

  def completion(params: TextDocumentPositionParams, ensimeActor: ActorRef, documentManager: TextDocumentManager, log: Logger)(implicit T: Timeout): Task[CompletionList] = {

    documentManager.documentForUri(params.textDocument.uri).map { doc =>

      Task.fromFuture(ensimeActor ? CompletionsReq(
        EnsimeLanguageServer.toSourceFileInfo(params.textDocument.uri,
          Some(new String(doc.text.toArray))),
        org.ensime.lsp.core.TextDocument.positionToOffset(doc.text.toArray, params.position, params.textDocument.uri),
        100,
        caseSens = false,
        reload = false
      )).map {
        case CompletionInfoList(prefix, completions) =>
          log.debug(s"Received ${completions.size} completions: ${completions.take(10).map(_.name)}")
          val completionList = completions
            .sortBy(-_.relevance)
            .map(toCompletion)

          CompletionList(false, completionList.toSeq)
      }
    }.getOrElse(Task(CompletionList(false, Nil)))
  }

  def documentSymbol(params: DocumentSymbolParams, ensimeActor: ActorRef, documentManager: TextDocumentManager, log: Logger)(implicit T: Timeout): Task[List[SymbolInformation]] = {
    documentManager.documentForUri(params.textDocument.uri).map { doc =>

      log.info(s"Document Symbols request for ${params.textDocument.uri}")
      val task = Task.fromFuture(ensimeActor ? StructureViewReq(
        EnsimeLanguageServer.toSourceFileInfo(params.textDocument.uri, Some(doc.text))
      ))
      task.map {
        case StructureView(members) =>
          log.debug(s"got back $members")
          members.flatMap(m => toSymbolInformation(params.textDocument.uri, doc.text, m, None, log))
      }
    }.getOrElse(Task(Nil))
  }

  val KeywordToKind = Map(
    "class"   -> SymbolKind.Class,
    "trait"   -> SymbolKind.Interface,
    "type"    -> SymbolKind.Interface,
    "package" -> SymbolKind.Package,
    "def"     -> SymbolKind.Method,
    "val"     -> SymbolKind.Constant,
    "var"     -> SymbolKind.Field
  )

  def toSymbolInformation(
    uri: String,
    text: String,
    structure: StructureViewMember,
    outer: Option[String],
    log: Logger
  ): Seq[SymbolInformation] =
    structure match {
      case StructureViewMember(keyword, name, pos, members) =>
        val kind = KeywordToKind
          .getOrElse(keyword, SymbolKind.Field)
        val rest =
          members.flatMap(m => toSymbolInformation(uri, text, m, Some(name), log))
        val position = pos match {
          case OffsetSourcePosition(_, offset) =>
            TextDocument.offsetToPosition(uri, text.toArray, offset)
          case LineSourcePosition(_, line) => Position(line, 0)
          case _ =>
            log.error(s"Unknown position for $name: $pos")
            Position(0, 0)
        }

        SymbolInformation(
          name,
          kind,
          Location(uri,
            Range(position,
              position.copy(
                character = position.character + name.length
              ))),
          outer
        ) +: rest

      case _ =>
        log.error(s"Unknown structure element: $structure")
        Seq.empty
    }


  def toCompletion(completionInfo: CompletionInfo) = {
    def symKind: Option[CompletionItemKind] = completionInfo.typeInfo map { info =>
      info.declaredAs match {
        case DeclaredAs.Method => CompletionItemKind.Method
        case DeclaredAs.Class  => CompletionItemKind.Class
        case DeclaredAs.Field  => CompletionItemKind.Field
        case DeclaredAs.Interface | DeclaredAs.Trait =>
          CompletionItemKind.Interface
        case DeclaredAs.Object => CompletionItemKind.Module
        case _                 => CompletionItemKind.Value
      }
    }

    CompletionItem(
      label = completionInfo.name,
      kind = symKind,
      detail = completionInfo.typeInfo.map(_.fullName)
    )
  }
}
