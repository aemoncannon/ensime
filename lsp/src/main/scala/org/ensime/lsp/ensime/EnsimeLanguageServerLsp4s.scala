// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import akka.actor.{ ActorRef, ActorSystem}
import akka.util.Timeout
import org.ensime.lsp.core.TextDocumentManager
import org.ensime.lsp.api.commands._
import monix.eval.Task

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.meta.jsonrpc.{Services, Response}
import scala.meta.lsp.{Lifecycle, InitializeResult, ServerCapabilities, CompletionOptions, TextDocument, InitializeParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DidChangeTextDocumentParams, Hover, DidCloseTextDocumentParams, TextDocumentPositionParams, CompletionList, PublishDiagnostics}
import scribe.Logger

import io.circe.{Json, JsonObject}
import io.circe.syntax._


class EnsimeLanguageServerLsp4s(log: Logger) {
  private var system: ActorSystem      = _
  private var fileStore: TempFileStore = _
  private var projectPath: String      = _
  private var ensimeActor: ActorRef = _

  private val documentManager = new TextDocumentManager

  implicit val timeout: Timeout     = Timeout(5 seconds)

  def services: Services = Services.empty(log)
    .requestAsync(Lifecycle.initialize)(initializeService)
    .notification(Lifecycle.initialized)(initialized)
    .requestAsync(Lifecycle.shutdown)(shutdownService)
    .notificationAsync(TextDocument.didOpen)(didOpenService)
    .notificationAsync(TextDocument.didClose)(didCloseService)
    .requestAsync(TextDocument.hover)(hoverService)
    .notificationAsync(TextDocument.didSave)(didSave)
    .notificationAsync(TextDocument.didChange)(didChange)
    .requestAsync(TextDocument.completion)(completion)

  /** Initializes the Ensime lsp server */
  private def initializeService(params: InitializeParams
  ): Task[Either[Response.Error, InitializeResult]] = {

    Task {
      log.info(s"Initializing with ${params.processId}, ${params.rootPath}, ${params.capabilities}")
    }.flatMap { _ =>
      LifecycleServices.initialize(params, this, log)
    }.map(_.map {
      case (actorSystem, tempFileStore, actor) =>
        log.info(s"Initialized with ${params.processId}, ${params.rootPath}, ${params.capabilities}")

        system = actorSystem
        fileStore = tempFileStore
        projectPath = params.rootPath
        ensimeActor = actor

        InitializeResult(ServerCapabilities(
          completionProvider = Some(CompletionOptions(false, Seq("."))),
          definitionProvider = true,
          hoverProvider = true,
          codeActionProvider = false,
          documentSymbolProvider = true
        ))
    })
  }

  /** Called when the client has received initialization parameters */
  private def initialized(params: Json): Unit = {
    log.info("Lsp client has confirmed initialization")
  }

  /** Shuts down the Ensime lsp server */
  private def shutdownService(params: Json): Task[Either[Response.Error, Json]] = {
    LifecycleServices.shutdown(system, log).map(_ => Right(JsonObject.empty.asJson))
  }

  /** A text document has been opened on the client */
  private def didOpenService(params: DidOpenTextDocumentParams): Task[Unit] = {
    TextDocumentServices.didOpen(params, ensimeActor, fileStore, documentManager, log).onErrorRecoverWith { case err =>
      log.error("Error encountered on didOpen", err)
      Task.raiseError(err)
    }
  }

  /** A text document has been closed on the client */
  private def didCloseService(params: DidCloseTextDocumentParams): Task[Unit] = {
    TextDocumentServices.didClose(params, ensimeActor, documentManager, log)
  }

  /** The cursor is hovering over a position in an open text document */
  private def hoverService(params: TextDocumentPositionParams): Task[Either[Response.Error, Hover]] = {
    TextDocumentServices.hover(params, ensimeActor, documentManager, log)
      .map(Right(_))
  }

  private def didSave(params: DidSaveTextDocumentParams): Task[Unit] = {
    TextDocumentServices.didSave(params, log)
  }

  private def didChange(params: DidChangeTextDocumentParams): Task[Unit] = {
    TextDocumentServices.didChange(params, ensimeActor, log)
  }

  private def completion(params: TextDocumentPositionParams): Task[Either[Response.Error, CompletionList]] = {
    TextDocumentServices.completion(params, ensimeActor, documentManager, log).map(Right(_))
  }

  // go to definition
  // document symbols
  // will save
  // will save wait until
  // references
  // code action
  // document highlight
  // formatting
  // rename
  // signature help

  // TODO: This is sent from the server to the client, so how does it hook in?
  def publishDiagnosticsService(diagnostics: PublishDiagnostics): Task[Either[Response.Error, Unit]] = ???
}
