// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import akka.actor.{ ActorRef, ActorSystem}
import akka.util.Timeout
import monix.eval.Task

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.meta.jsonrpc.{Services, Response}
import scala.meta.lsp.{Lifecycle, InitializeResult, ServerCapabilities, CompletionOptions, TextDocument, InitializeParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DidChangeTextDocumentParams, Hover, DidCloseTextDocumentParams, TextDocumentPositionParams, CompletionList, PublishDiagnostics, SymbolInformation, DocumentSymbolParams, TextDocumentSyncOptions, TextDocumentSyncKind, Workspace}
import scribe.Logger
import monix.eval.{Task, MVar}

import io.circe.{Json, JsonObject}
import io.circe.syntax._

/**
  * Provides the [[Services]] for a [[scala.meta.jsonrpc.LanguageServer]].
  *
  * On receiving an [[Lifecycle.initialize]] message an EnsimeActor is created.  This actor is communicated with when processing all subsequent messages.
  */
final class EnsimeServices(log: Logger, documentServices: TextDocumentServices,
  initialState: OptionalRef[(EnsimeProjectWrapper, EnsimeCache)], system: OptionalRef[ActorSystem]) {

  implicit val timeout: Timeout     = Timeout(5 seconds)

  def services: Services = Services.empty(log)
    .requestAsync(Lifecycle.initialize)(initializeService)
    .notification(Lifecycle.initialized)(initialized)
    .requestAsync(Lifecycle.shutdown)(shutdownService)
    .notificationAsync(TextDocument.didOpen)(documentServices.didOpen)
    .notificationAsync(TextDocument.didClose)(documentServices.didClose)
    .requestAsync(TextDocument.hover)(documentServices.hover)
    .notificationAsync(TextDocument.didSave)(documentServices.didSave)
    .notificationAsync(TextDocument.willSave)(documentServices.willSave)
    .requestAsync(TextDocument.willSaveWaitUntil)(documentServices.willSaveWaitUntil)
    .notificationAsync(TextDocument.didChange)(documentServices.didChange)
    .requestAsync(TextDocument.completion)(documentServices.completion)
    .requestAsync(TextDocument.documentSymbol)(documentServices.documentSymbol)
    .requestAsync(TextDocument.definition)(documentServices.definition)
    // .notificationAsync(Workspace.didChangeWatchedFiles)(???)

  /** Initializes the Ensime lsp server */
  private def initializeService(params: InitializeParams
  ): Task[Either[Response.Error, InitializeResult]] = {

    Task {
      log.info(s"Initializing with ${params.processId}, ${params.rootPath}, ${params.capabilities}")
    }.flatMap { _ =>
      LifecycleServices.initialize(params, this, log)
    }.flatMap {
       case Right( (actorSystem, ensimeCache, actor) ) =>
        log.info(s"Initialized with ${params.processId}, ${params.rootPath}, ${params.capabilities}")

        initialState.put((EnsimeProjectWrapper(actor), ensimeCache))
          .flatMap { _ => system.put(actorSystem)}
          .map { _ =>

        Right(InitializeResult(ServerCapabilities(
          completionProvider = Some(CompletionOptions(false, Seq("."))),
          definitionProvider = true,
          hoverProvider = true,
          codeActionProvider = false,
          documentSymbolProvider = true,
          textDocumentSync = Some(TextDocumentSyncOptions(change = Some(TextDocumentSyncKind.Full)))
        )))
        }

      case Left(err) => Task(Left(err))
    }
  }

  /** Called when the client has received initialization parameters */
  private def initialized(params: Json): Unit = {
    log.info("Lsp client has confirmed initialization")
  }

  /** Shuts down the Ensime lsp server */
  private def shutdownService(params: Json): Task[Either[Response.Error, Json]] = {
    system.get.flatMap {
      case Left(_) => Task(Left(Response.internalError("Ensime not initialized yet")))
      case Right(actorSystem) =>
        LifecycleServices.shutdown(actorSystem, log).map(_ => Right(JsonObject.empty.asJson))
    }
  }
  // TODO: This is sent from the server to the client, so how does it hook in?
  def publishDiagnosticsService(diagnostics: PublishDiagnostics): Task[Either[Response.Error, Unit]] = ???
}

object EnsimeServices {

  def apply(log: Logger): Task[EnsimeServices] = {
    for {
      initialState <- OptionalRef.empty[(EnsimeProjectWrapper, EnsimeCache)]
      system <- OptionalRef.empty[ActorSystem]
      textDocumentServices <- TextDocumentServices(initialState, log)
    } yield {
      new EnsimeServices(log, textDocumentServices, initialState, system)
    }
  }
}
