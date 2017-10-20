// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.api.methods

import org.ensime.lsp.JsonUtils
import org.ensime.lsp.rpc.companions._
import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.types.{
  MarkdownString,
  MarkedString,
  RawMarkedString
}
import spray.json._

import scala.util.{ Failure, Success, Try }

private[lsp] object ServerCommandConversions
    extends DefaultJsonProtocol
    with FamilyFormats {

  implicit val textDocumentDefinitionRequestFormat
    : JsonFormat[TextDocumentDefinitionRequest] =
    JsonUtils.wrapperFormat(TextDocumentDefinitionRequest.apply, _.params)

  implicit val textDocumentHoverRequestFormat
    : JsonFormat[TextDocumentHoverRequest] =
    JsonUtils.wrapperFormat(TextDocumentHoverRequest.apply, _.params)

  implicit val textDocumentCompletionRequestFormat
    : JsonFormat[TextDocumentCompletionRequest] =
    JsonUtils.wrapperFormat(TextDocumentCompletionRequest.apply, _.params)
}

object ServerCommands {
  import ServerCommandConversions._

  implicit val initializeCommand: RpcCommand[InitializeParams] =
    RpcCommand[InitializeParams]("initialize")
  implicit val shutdownCommand: RpcCommand[Shutdown] =
    RpcCommand[Shutdown]("shutdown")
  implicit val competitionCommand: RpcCommand[TextDocumentCompletionRequest] =
    RpcCommand[TextDocumentCompletionRequest]("textDocument/completion")
  implicit val definitionCommand: RpcCommand[TextDocumentDefinitionRequest] =
    RpcCommand[TextDocumentDefinitionRequest]("textDocument/definition")
  implicit val hoverCommand: RpcCommand[TextDocumentHoverRequest] =
    RpcCommand[TextDocumentHoverRequest]("textDocument/hover")
  implicit val documentSymbolCommand: RpcCommand[DocumentSymbolParams] =
    RpcCommand[DocumentSymbolParams]("textDocument/documentSymbol")
}

object ServerCommand extends CommandCompanion[ServerCommand] {

  import ServerCommands._

  val commands = Seq(
    initializeCommand,
    shutdownCommand,
    competitionCommand,
    definitionCommand,
    hoverCommand,
    documentSymbolCommand
  )
}

object ClientCommands {
  import FamilyFormats._

  implicit val showMessageRequestCommand: RpcCommand[ShowMessageRequestParams] =
    RpcCommand[ShowMessageRequestParams]("showMessageRequest")
}

object ClientCommand extends CommandCompanion[ClientCommand] {
  import ClientCommands._

  val commands = Seq(showMessageRequestCommand)
}

object Notifications {
  import FamilyFormats._

  implicit val showMessageNotification: RpcNotification[ShowMessageParams] =
    RpcNotification[ShowMessageParams]("window/showMessage")
  implicit val logMessageNotification: RpcNotification[LogMessageParams] =
    RpcNotification[LogMessageParams]("window/logMessage")
  implicit val publishDiagnosticsNotification
    : RpcNotification[PublishDiagnostics] =
    RpcNotification[PublishDiagnostics]("textDocument/publishDiagnostics")
  implicit val didOpenNotification: RpcNotification[DidOpenTextDocumentParams] =
    RpcNotification[DidOpenTextDocumentParams]("textDocument/didOpen")
  implicit val didChangeNotification
    : RpcNotification[DidChangeTextDocumentParams] =
    RpcNotification[DidChangeTextDocumentParams]("textDocument/didChange")
  implicit val didCloseNotification
    : RpcNotification[DidCloseTextDocumentParams] =
    RpcNotification[DidCloseTextDocumentParams]("textDocument/didClose")
  implicit val didSaveNotification: RpcNotification[DidSaveTextDocumentParams] =
    RpcNotification[DidSaveTextDocumentParams]("textDocument/didSave")
  implicit val didChangeWatchedFilesNotification
    : RpcNotification[DidChangeWatchedFiles] =
    RpcNotification[DidChangeWatchedFiles]("workspace/didChangeWatchedFiles")
  implicit val initializedNotification: RpcNotification[Initialized] =
    RpcNotification[Initialized]("initialized")
  implicit val cancelRequestNotification: RpcNotification[CancelRequest] =
    RpcNotification[CancelRequest]("$/cancelRequest")
}

object Notification extends NotificationCompanion[Notification] {

  import Notifications._

  val notifications = Seq(
    showMessageNotification,
    logMessageNotification,
    publishDiagnosticsNotification,
    didOpenNotification,
    didChangeNotification,
    didCloseNotification,
    didSaveNotification,
    didChangeWatchedFilesNotification,
    initializedNotification,
    cancelRequestNotification
  )
}

private[lsp] object RpcResponseConversions
    extends DefaultJsonProtocol
    with FamilyFormats {

  implicit val markedStringFormat: JsonFormat[MarkedString] =
    JsonFormat.instance[MarkedString]({
      case raw: RawMarkedString     => raw.toJson
      case markdown: MarkdownString => markdown.toJson
    })(
      j =>
        Try(j.convertTo[RawMarkedString]) orElse
          Try(j.convertTo[MarkdownString]) match {
          case Failure(e) => sys.error(s"Wrong MarkedString format for $j")
          case Success(x) => x
      }
    )
  implicit val definitionResultFormat: JsonFormat[DefinitionResult] =
    JsonUtils.wrapperFormat(DefinitionResult, _.params)
  implicit val documentSymbolResultFormat: JsonFormat[DocumentSymbolResult] =
    JsonUtils.wrapperFormat(DocumentSymbolResult, _.params)
}
