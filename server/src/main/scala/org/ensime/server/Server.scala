// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.net.InetSocketAddress
import java.nio.file.Paths

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util._

import akka.actor._
import akka.actor.SupervisorStrategy.Stop
import com.typesafe.config._
import io.netty.channel.Channel
import org.ensime.AkkaBackCompat
import org.ensime.api._
import org.ensime.config._
import org.ensime.config.richconfig._
import org.ensime.core._
import org.ensime.server.tcp.TCPServer
import org.ensime.util.Slf4jSetup
import org.ensime.util.ensimefile.Implicits.DefaultCharset
import org.ensime.util.path._
import org.slf4j._

class ServerActor(
  config: EnsimeConfig,
  serverConfig: EnsimeServerConfig,
  protocol: Protocol
) extends Actor
    with ActorLogging {

  var channel: Channel = _

  override val supervisorStrategy = OneForOneStrategy() {
    case ex: Exception =>
      log.error(ex, s"Error with monitor actor ${ex.getMessage}")
      self ! ShutdownRequest(
        s"Monitor actor failed with ${ex.getClass} - ${ex.toString}",
        isError = true
      )
      Stop
  }

  def initialiseChildren(): Unit = {

    implicit val config: EnsimeConfig             = this.config
    implicit val serverConfig: EnsimeServerConfig = this.serverConfig

    val broadcaster = context.actorOf(Broadcaster(), "broadcaster")
    val project     = context.actorOf(Project(broadcaster), "project")

    val preferredTcpPort = PortUtil.port(config.cacheDir.file, "port")
    context.actorOf(Props(
                      new TCPServer(
                        config.cacheDir.file.toFile,
                        protocol,
                        project,
                        broadcaster,
                        serverConfig.shutDownOnDisconnect,
                        preferredTcpPort
                      )
                    ),
                    "tcp-server")

    // async start the HTTP Server
    val selfRef           = self
    val preferredHttpPort = PortUtil.port(config.cacheDir.file, "http")

    val hookHandlers: WebServer.HookHandlers = { outHandler =>
      val delegate = context.actorOf(Props(new Actor {
        def receive: Receive = {
          case res: RpcResponseEnvelope => outHandler(res)
        }
      }))
      val inHandler =
        context.actorOf(ConnectionHandler(project, broadcaster, delegate))

      { req =>
        inHandler ! req
      }
    }

    val docs = DocJarReading.forConfig(config)
    WebServer
      .start(docs, preferredHttpPort.getOrElse(0), hookHandlers)
      .onComplete {
        case Failure(ex) =>
          log.error(ex, s"Error binding http endpoint ${ex.getMessage}")
          selfRef ! ShutdownRequest(
            s"http endpoint failed to bind ($preferredHttpPort)",
            isError = true
          )

        case Success(ch) =>
          this.channel = ch
          log.info(s"ENSIME HTTP on ${ch.localAddress()}")
          try {
            val port =
              ch.localAddress().asInstanceOf[InetSocketAddress].getPort()
            PortUtil.writePort(config.cacheDir.file, port, "http")
          } catch {
            case ex: Throwable =>
              log.error(ex,
                        s"Error initializing http endpoint ${ex.getMessage}")
              selfRef ! ShutdownRequest(
                s"http endpoint failed to initialise: ${ex.getMessage}",
                isError = true
              )
          }
      }(context.system.dispatcher)

    Environment.info foreach log.info
  }

  override def preStart(): Unit =
    try {
      initialiseChildren()
    } catch {
      case t: Throwable =>
        log.error(t, s"Error during startup - ${t.getMessage}")
        self ! ShutdownRequest(t.toString, isError = true)
    }
  override def receive: Receive = {
    case req: ShutdownRequest =>
      triggerShutdown(req)
  }

  def triggerShutdown(request: ShutdownRequest): Unit =
    Server.shutdown(context.system, channel, request, serverConfig.exit)

}
object ServerActor {
  def props(protocol: Protocol)(implicit
                                ensimeConfig: EnsimeConfig,
                                serverConfig: EnsimeServerConfig): Props =
    Props(new ServerActor(ensimeConfig, serverConfig, protocol))
}

object Server extends AkkaBackCompat {
  Slf4jSetup.init()

  val log = LoggerFactory.getLogger("Server")

  // Config is loaded in this order:
  //
  //   1. system properties
  //   2. .ensime-server.conf beside .ensime
  //   3. .ensime-server.conf in the XDG / user.home
  //   4. bundled application.conf
  def loadConfig(): Config = {
    val fallback = ConfigFactory.load()
    val user = List(
      parseServerConfig(fallback).config.file.getParent,
      Paths.get(
        sys.env.get("XDG_CONFIG_HOME").getOrElse(sys.props("user.home"))
      )
    ).map(_ / ".ensime-server.conf")
      .filter(_.exists())
      .map(p => ConfigFactory.parseFile(p.toFile))

    (ConfigFactory.systemProperties() :: user ::: fallback :: Nil).reduce {
      (higher, lower) =>
        higher.withFallback(lower)
    }
  }

  def main(args: Array[String]): Unit = {
    val config                                    = loadConfig()
    implicit val serverConfig: EnsimeServerConfig = parseServerConfig(config)
    implicit val ensimeConfig: EnsimeConfig =
      EnsimeConfigProtocol.parse(serverConfig.config.file.readString())

    Canon.config = ensimeConfig
    Canon.serverConfig = serverConfig

    val protocol: Protocol = serverConfig.protocol match {
      case "swanki" => new SwankiProtocol
      case "swank"  => new SwankProtocol
      case other =>
        throw new IllegalArgumentException(
          s"$other is not a valid ENSIME protocol"
        )
    }

    ActorSystem
      .create("ENSIME", config)
      .actorOf(ServerActor.props(protocol), "ensime-main")
  }

  def shutdown(system: ActorSystem,
               channel: Channel,
               request: ShutdownRequest,
               exit: Boolean): Unit = {
    val t = new Thread(new Runnable {
      def run(): Unit = {
        if (request.isError)
          log.error(
            s"Shutdown requested due to internal error: ${request.reason}"
          )
        else
          log.info(s"Shutdown requested: ${request.reason}")

        log.info("Shutting down the ActorSystem")
        Try(system.terminate())

        log.info("Awaiting actor system termination")
        Try(Await.result(system.whenTerminated, Duration.Inf))

        log.info("Shutting down the Netty channel")
        Try(channel.close().sync())

        log.info("Shutdown complete")
        if (exit) {
          if (request.isError)
            System.exit(1)
          else
            System.exit(0)
        }
      }
    })
    t.setName("Server Shutdown")
    t.setDaemon(true)
    t.start()
  }
}
