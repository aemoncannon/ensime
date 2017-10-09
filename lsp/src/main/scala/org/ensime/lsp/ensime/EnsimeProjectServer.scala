package org.ensime.lsp.ensime

import akka.actor.Actor
import akka.event.slf4j.SLF4JLogging
import akka.util.Timeout
import org.ensime.api._
import org.ensime.core.{ Broadcaster, Project }

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

class EnsimeProjectServer(langServer: EnsimeLanguageServer,
                          implicit val config: EnsimeConfig,
                          implicit val ensimeServerConfig: EnsimeServerConfig)
    extends Actor
    with SLF4JLogging {
  implicit val timeout: Timeout = Timeout(10 seconds)

  val broadcaster = context.actorOf(Broadcaster(), "broadcaster")
  val project     = context.actorOf(Project(broadcaster), "project")

  override def preStart(): Unit =
    broadcaster ! Broadcaster.Register

  override def postStop(): Unit = {
    super.postStop()
    log.info("Shutting down.")
  }

  private val compilerDiagnostics: ListBuffer[Note] = ListBuffer.empty

  override def receive = {
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
