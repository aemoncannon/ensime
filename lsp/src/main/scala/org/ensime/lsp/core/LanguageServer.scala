// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import java.io.{ InputStream, OutputStream }

import akka.event.slf4j.SLF4JLogging
import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.types._
import org.ensime.lsp.rpc.companions.RpcResponse
import org.ensime.lsp.rpc.messages._

object LanguageServer {

  object ResponseHandler {
    def buildHandler(
      l: LanguageServer
    ): PartialFunction[
      (String, ServerCommand, CorrelationId),
      JsonRpcResponseSuccessMessage
    ] = {
      case (_, InitializeParams(pid, rootPath, capabilities), id) =>
        RpcResponse.write(
          InitializeResult(l.initialize(pid, rootPath, capabilities)),
          id
        )
      case ("textDocument/completion",
            TextDocumentCompletionRequest(
              TextDocumentPositionParams(textDocument, position)
            ),
            id) =>
        RpcResponse
          .write(l.completionRequest(textDocument, position), id)
      case ("textDocument/definition",
            TextDocumentDefinitionRequest(
              TextDocumentPositionParams(textDocument, position)
            ),
            id) =>
        RpcResponse
          .write(l.gotoDefinitionRequest(textDocument, position), id)
      case ("textDocument/hover",
            TextDocumentHoverRequest(
              TextDocumentPositionParams(textDocument, position)
            ),
            id) =>
        RpcResponse.write(l.hoverRequest(textDocument, position), id)
      case ("textDocument/documentSymbol", DocumentSymbolParams(tdi), id) =>
        RpcResponse
          .write(DocumentSymbolResult(l.documentSymbols(tdi)), id)
      case ("shutdown", Shutdown(), id) =>
        l.shutdown()
        JsonRpcResponseSuccessMessage(spray.json.JsNull, id)
    }
  }
}

/**
 * A language server implementation. Users should subclass this class and implement specific behavior.
 */
abstract class LanguageServer(inStream: InputStream, outStream: OutputStream)
    extends SLF4JLogging {

  protected val documentManager = new TextDocumentManager

  private val notificationHandler: Notification => Unit = {
    case DidOpenTextDocumentParams(td) =>
      onOpenTextDocument(td)
    case DidChangeTextDocumentParams(td, changes) =>
      onChangeTextDocument(td, changes)
    case DidSaveTextDocumentParams(td) =>
      onSaveTextDocument(td)
    case DidCloseTextDocumentParams(td) =>
      onCloseTextDocument(td)
    case DidChangeWatchedFiles(changes) =>
      onChangeWatchedFiles(changes)
    case e => log.error(s"Unknown notification $e")
  }

  val connection: Connection = new Connection(
    inStream,
    outStream,
    Seq(documentManager.notificationHandler, notificationHandler),
    (method, command, id) =>
      LanguageServer.ResponseHandler
        .buildHandler(this)
        .lift
        .andThen(_.getOrElse({
          log.error(s"Unknown command $method $command $id")
          sys.error("Unknown command")
        }))((method, command, id))
  )

  def start(): Unit =
    connection.start()

  def onOpenTextDocument(td: TextDocumentItem): Unit

  def onChangeTextDocument(td: VersionedTextDocumentIdentifier,
                           changes: Seq[TextDocumentContentChangeEvent]): Unit

  def onSaveTextDocument(td: TextDocumentIdentifier): Unit

  def onCloseTextDocument(td: TextDocumentIdentifier): Unit

  def onChangeWatchedFiles(changes: Seq[FileEvent]): Unit

  def initialize(pid: Long,
                 rootPath: String,
                 capabilities: ClientCapabilities): ServerCapabilities

  def completionRequest(textDocument: TextDocumentIdentifier,
                        position: Position): CompletionList

  def shutdown(): Unit

  def gotoDefinitionRequest(textDocument: TextDocumentIdentifier,
                            position: Position): DefinitionResult

  def hoverRequest(textDocument: TextDocumentIdentifier,
                   position: Position): Hover

  def documentSymbols(tdi: TextDocumentIdentifier): Seq[SymbolInformation]
}
