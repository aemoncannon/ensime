// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import org.ensime.api.{ EnsimeConfig, EnsimeServerConfig }
import org.ensime.lsp.api.commands.MessageType

import scala.concurrent.duration._

/**
 * An actor that instantiates the Ensime Server actor and supervises it.
 *
 * It catches `ActorInitializationError` and tries to restart it.
 */
class EnsimeActor(langServer: EnsimeLanguageServer,
                  config: EnsimeConfig,
                  ensimeServerConfig: EnsimeServerConfig)
    extends Actor
    with SLF4JLogging {

  private var project: ActorRef = _

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy(5, 1 minute) {
      case e @ ActorInitializationException(actor, message, cause) =>
        log.error(s"Actor failed to initialize", e)
        langServer.connection.logMessage(MessageType.Error,
                                         s"Error starting ensime: $message")
        SupervisorStrategy.Restart
      case e =>
        log.error(s"Actor crashed: ", e)
        SupervisorStrategy.Restart
    }

  override def receive = {
    case message =>
      if (project eq null) {
        // trying to catch this elusive ActorInitializationException by creating this actor as late
        // as possible. Still, it looks like the supervisor strategy does not get this crash
        log.info("Starting problematic actor now")
        project = context.actorOf(
          Props(
            new EnsimeProjectServer(langServer, config, ensimeServerConfig)
          ),
          "ensimeProject"
        )
        log.info(s"Created: $project")
      }

      project forward message
  }
}
