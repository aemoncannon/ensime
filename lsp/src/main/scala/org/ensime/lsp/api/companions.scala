// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.api.companions

import org.ensime.lsp.rpc.companions._
import org.ensime.lsp.api.commands._
import spray.json._

object ServerCommands {
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
  implicit val showMessageRequestCommand: RpcCommand[ShowMessageRequestParams] =
    RpcCommand[ShowMessageRequestParams]("showMessageRequest")
}

object ClientCommand extends CommandCompanion[ClientCommand] {
  import ClientCommands._

  val commands = Seq(showMessageRequestCommand)
}

object Notifications {
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
