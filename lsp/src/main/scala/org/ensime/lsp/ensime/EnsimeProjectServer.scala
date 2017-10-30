// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import akka.actor.Actor
import akka.event.slf4j.SLF4JLogging
import org.ensime.api._
import org.ensime.core.{ Broadcaster, Project }

import scala.collection.mutable.ListBuffer

class EnsimeProjectServer(langServer: EnsimeLanguageServer,
                          implicit val config: EnsimeConfig,
                          implicit val ensimeServerConfig: EnsimeServerConfig)
    extends Actor
    with SLF4JLogging {

  private val broadcaster = context.actorOf(Broadcaster(), "broadcaster")
  private val project     = context.actorOf(Project(broadcaster), "project")

  override def preStart(): Unit =
    broadcaster ! Broadcaster.Register

  override def postStop(): Unit =
    broadcaster ! Broadcaster.Unregister

  private val compilerDiagnostics: ListBuffer[Note] = ListBuffer.empty

  override def receive: PartialFunction[Any, Unit] = {
    case ClearAllScalaNotesEvent =>
      compilerDiagnostics.clear()

    case NewScalaNotesEvent(isFull, notes) =>
      compilerDiagnostics ++= notes
      publishDiagnostics()

    case message =>
      log.debug(s"Forwarding $message")
      project forward message
  }

  private def publishDiagnostics(): Unit = {
    log.debug(s"Scala notes: ${compilerDiagnostics.mkString("\n")}")
    langServer.publishDiagnostics(compilerDiagnostics.toList)
  }
}
