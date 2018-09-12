// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import akka.util.Timeout
import monix.eval.Task

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.meta.jsonrpc.{ Response, Services, LanguageClient }
import scala.meta.lsp.{ Lifecycle, PublishDiagnostics, TextDocument }
import scribe.Logger
import monix.eval.Task

/**
 * Provides the [[Services]] for a [[scala.meta.jsonrpc.LanguageServer]].
 */
final class EnsimeServices(languageClient: LanguageClient,
                           log: Logger,
                           lifecycleServices: LifecycleServices,
                           documentServices: TextDocumentServices) {

  implicit val timeout: Timeout = Timeout(5 seconds)

  def services: Services =
    Services
      .empty(log)
      .requestAsync(Lifecycle.initialize)(lifecycleServices.initialize)
      .notification(Lifecycle.initialized)(lifecycleServices.initialized)
      .requestAsync(Lifecycle.shutdown)(_ => lifecycleServices.shutdown)
      .notificationAsync(TextDocument.didOpen)(documentServices.didOpen)
      .notificationAsync(TextDocument.didClose)(documentServices.didClose)
      .requestAsync(TextDocument.hover)(documentServices.hover)
      .notificationAsync(TextDocument.didSave)(documentServices.didSave)
      .notificationAsync(TextDocument.willSave)(documentServices.willSave)
      .requestAsync(TextDocument.willSaveWaitUntil)(
        documentServices.willSaveWaitUntil
      )
      .notificationAsync(TextDocument.didChange)(documentServices.didChange)
      .requestAsync(TextDocument.completion)(documentServices.completion)
      .requestAsync(TextDocument.documentSymbol)(
        documentServices.documentSymbol
      )
      .requestAsync(TextDocument.definition)(documentServices.definition)

  // TODO: This is sent from the server to the client, so how does it hook in?
  def publishDiagnosticsService(
    diagnostics: PublishDiagnostics
  ): Task[Either[Response.Error, Unit]] = ???
}

object EnsimeServices {

  /**
   * Creates the [[EnsimeServices]].   The initial ensime state is empty as the
   * ensime system only created when an "initialize" request is received.
   */
  def apply(languageClient: LanguageClient, log: Logger): Task[EnsimeServices] =
    for {
      initialState         <- OptionalRef.empty[EnsimeState]
      textDocumentServices <- TextDocumentServices(initialState.get, log)
    } yield {
      val lifecycleServices = new LifecycleServices(initialState, log)
      new EnsimeServices(languageClient, log, lifecycleServices, textDocumentServices)
    }
}
