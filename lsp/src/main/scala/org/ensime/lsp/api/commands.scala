// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.api.commands

import org.ensime.lsp.api.types._

import spray.json._
import scalaz.deriving

object TextDocumentSyncKind {

  /**
   * Documents should not be synced at all.
   */
  final val None = 0

  /**
   * Documents are synced by always sending the full content
   * of the document.
   */
  final val Full = 1

  /**
   * Documents are synced by sending the full content on open.
   * After that only incremental updates to the document are
   * send.
   */
  final val Incremental = 2
}

object MessageType {

  /** An error message. */
  final val Error = 1

  /** A warning message. */
  final val Warning = 2

  /** An information message. */
  final val Info = 3

  /** A log message. */
  final val Log = 4
}

sealed trait Message
sealed trait ServerCommand extends Message
sealed trait ClientCommand extends Message

sealed trait Response       extends Message
sealed trait ResultResponse extends Response

sealed trait Notification extends Message

/**
 * Parameters and types used in the `initialize` message.
 */
@deriving(JsReader, JsWriter)
case class InitializeParams(
                            // The process Id of the parent process that started the server.
                            processId: Long,
                            //The rootPath of the workspace. Is null if no folder is open.
                            rootPath: String,
                            //The capabilities provided by the client (editor)
                            capabilities: ClientCapabilities)
    extends ServerCommand

case class InitializeError(retry: Boolean)

@deriving(JsReader, JsWriter)
case class ClientCapabilities()

@deriving(JsReader, JsWriter)
case class ServerCapabilities(
  //Defines how text documents are synced.
  textDocumentSync: Int = TextDocumentSyncKind.Full,
  //The server provides hover support.
  hoverProvider: Boolean = false,
  //The server provides completion support.
  completionProvider: Option[CompletionOptions],
  //The server provides signature help support.
  signatureHelpProvider: Option[SignatureHelpOptions] = None,
  //The server provides goto definition support.
  definitionProvider: Boolean = false,
  ///The server provides find references support.
  referencesProvider: Boolean = false,
  //The server provides document highlight support.
  documentHighlightProvider: Boolean = false,
  //The server provides document symbol support.
  documentSymbolProvider: Boolean = false,
  //The server provides workspace symbol support.
  workspaceSymbolProvider: Boolean = false,
  //The server provides code actions.
  codeActionProvider: Boolean = false,
  //The server provides code lens.
  codeLensProvider: Option[CodeLensOptions] = None,
  //The server provides document formatting.
  documentFormattingProvider: Boolean = false,
  //The server provides document range formatting.
  documentRangeFormattingProvider: Boolean = false,
  //The server provides document formatting on typing.
  documentOnTypeFormattingProvider: Option[DocumentOnTypeFormattingOptions] =
    None,
  //The server provides rename support.
  renameProvider: Boolean = false
)

@deriving(JsReader, JsWriter)
case class CompletionOptions(resolveProvider: Boolean,
                             triggerCharacters: Seq[String])

@deriving(JsReader, JsWriter)
case class SignatureHelpOptions(triggerCharacters: Seq[String])

@deriving(JsReader, JsWriter)
case class CodeLensOptions(resolveProvider: Boolean = false)

@deriving(JsReader, JsWriter)
case class DocumentOnTypeFormattingOptions(firstTriggerCharacter: String,
                                           moreTriggerCharacters: Seq[String])

@deriving(JsReader, JsWriter)
case class CompletionList(isIncomplete: Boolean, items: Seq[CompletionItem])
    extends ResultResponse

@deriving(JsReader, JsWriter)
case class InitializeResult(capabilities: ServerCapabilities)
    extends ResultResponse

@deriving(JsReader, JsWriter)
case class Shutdown() extends ServerCommand

case class ShutdownResult(dummy: Int) extends ResultResponse

@deriving(JsReader, JsWriter)
case class ShowMessageRequestParams(
                                    //The message type. @see MessageType
                                    tpe: Long,
                                    //The actual message
                                    message: String,
                                    //The message action items to present.
                                    actions: Seq[MessageActionItem])
    extends ClientCommand

/**
 * A short title like 'Retry', 'Open Log' etc.
 */
@deriving(JsReader, JsWriter)
case class MessageActionItem(title: String)

@deriving(JsReader, JsWriter)
case class TextDocumentPositionParams(textDocument: TextDocumentIdentifier,
                                      position: Position)
@deriving(JsReader, JsWriter)
case class DocumentSymbolParams(textDocument: TextDocumentIdentifier)
    extends ServerCommand

case class TextDocumentCompletionRequest(params: TextDocumentPositionParams)
    extends ServerCommand
object TextDocumentCompletionRequest {
  implicit val jsWriter: JsWriter[TextDocumentCompletionRequest] =
    JsWriter[TextDocumentPositionParams].contramap(_.params)
  implicit val jsReader: JsReader[TextDocumentCompletionRequest] =
    JsReader[TextDocumentPositionParams].map(TextDocumentCompletionRequest(_))
}

case class TextDocumentDefinitionRequest(params: TextDocumentPositionParams)
    extends ServerCommand
object TextDocumentDefinitionRequest {
  implicit val jsWriter: JsWriter[TextDocumentDefinitionRequest] =
    JsWriter[TextDocumentPositionParams].contramap(_.params)
  implicit val jsReader: JsReader[TextDocumentDefinitionRequest] =
    JsReader[TextDocumentPositionParams].map(TextDocumentDefinitionRequest(_))
}

case class TextDocumentHoverRequest(params: TextDocumentPositionParams)
    extends ServerCommand
object TextDocumentHoverRequest {
  implicit val jsWriter: JsWriter[TextDocumentHoverRequest] =
    JsWriter[TextDocumentPositionParams].contramap(_.params)
  implicit val jsReader: JsReader[TextDocumentHoverRequest] =
    JsReader[TextDocumentPositionParams].map(TextDocumentHoverRequest(_))
}

@deriving(JsReader, JsWriter)
case class Hover(contents: Seq[MarkedString], range: Option[Range])
    extends ResultResponse

///////////////////////////// Notifications ///////////////////////////////

// From server to client

@deriving(JsReader, JsWriter)
case class ShowMessageParams(tpe: Int, message: String) extends Notification
@deriving(JsReader, JsWriter)
case class LogMessageParams(tpe: Int, message: String) extends Notification
@deriving(JsReader, JsWriter)
case class PublishDiagnostics(uri: String, diagnostics: Seq[Diagnostic])
    extends Notification

// from client to server

case class ExitNotification() extends Notification
@deriving(JsReader, JsWriter)
case class DidOpenTextDocumentParams(textDocument: TextDocumentItem)
    extends Notification
@deriving(JsReader, JsWriter)
case class DidChangeTextDocumentParams(
  textDocument: VersionedTextDocumentIdentifier,
  contentChanges: Seq[TextDocumentContentChangeEvent]
) extends Notification

@deriving(JsReader, JsWriter)
case class DidCloseTextDocumentParams(textDocument: TextDocumentIdentifier)
    extends Notification
@deriving(JsReader, JsWriter)
case class DidSaveTextDocumentParams(textDocument: TextDocumentIdentifier)
    extends Notification
@deriving(JsReader, JsWriter)
case class DidChangeWatchedFiles(changes: Seq[FileEvent]) extends Notification
@deriving(JsReader, JsWriter)
case class Initialized() extends Notification
@deriving(JsReader, JsWriter)
case class CancelRequest(id: Int) extends Notification

@deriving(JsReader, JsWriter)
case class FileEvent(uri: String, `type`: Int)

object FileChangeType {
  final val Created = 1
  final val Changed = 2
  final val Deleted = 3
}

case class DocumentSymbolResult(params: Seq[SymbolInformation])
    extends ResultResponse
object DocumentSymbolResult {
  implicit val jsWriter: JsWriter[DocumentSymbolResult] =
    JsWriter[Seq[SymbolInformation]].contramap(_.params)
  implicit val jsReader: JsReader[DocumentSymbolResult] =
    JsReader[Seq[SymbolInformation]].map(DocumentSymbolResult(_))
}

case class DefinitionResult(params: Seq[Location]) extends ResultResponse
object DefinitionResult {
  implicit val jsWriter: JsWriter[DefinitionResult] =
    JsWriter[Seq[Location]].contramap(_.params)
  implicit val jsReader: JsReader[DefinitionResult] =
    JsReader[Seq[Location]].map(DefinitionResult(_))
}
