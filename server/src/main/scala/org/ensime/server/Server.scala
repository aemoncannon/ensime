// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.io._
import java.net.InetSocketAddress
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util._
import scala.util.Properties._

import akka.actor._
import akka.actor.SupervisorStrategy.Stop
import akka.util.Timeout
import com.google.common.base.Charsets
import com.google.common.io.Files
import io.netty.channel.Channel

import org.ensime.api._
import org.ensime.config._
import org.ensime.core._
import org.ensime.AkkaBackCompat
import org.ensime.server.tcp.TCPServer
import org.ensime.util.Slf4jSetup
import org.slf4j._

class ServerActor(
    config: EnsimeConfig,
    serverConfig: EnsimeServerConfig,
    protocol: Protocol,
    interface: String = "127.0.0.1"
) extends Actor with ActorLogging {

  var channel: Channel = _

  override val supervisorStrategy = OneForOneStrategy() {
    case ex: Exception =>
      log.error(ex, s"Error with monitor actor ${ex.getMessage}")
      self ! ShutdownRequest(s"Monitor actor failed with ${ex.getClass} - ${ex.toString}", isError = true)
      Stop
  }

  def initialiseChildren(): Unit = {

    implicit val config: EnsimeConfig = this.config
    implicit val timeout: Timeout = Timeout(10 seconds)
    implicit val ensimeServerConfig: EnsimeServerConfig = this.serverConfig
    val broadcaster = context.actorOf(Broadcaster(), "broadcaster")
    val project = context.actorOf(Project(broadcaster), "project")

    val preferredTcpPort = PortUtil.port(config.cacheDir, "port")
    val shutdownOnLastDisconnect = serverConfig.shutDownOnDisconnect
    context.actorOf(Props(
      new TCPServer(
        config.cacheDir, protocol, project,
        broadcaster, shutdownOnLastDisconnect, preferredTcpPort
      )
    ), "tcp-server")

    // async start the HTTP Server
    val selfRef = self
    val preferredHttpPort = PortUtil.port(config.cacheDir, "http")

    val hookHandlers: WebServer.HookHandlers = {
      outHandler =>
        val delegate = context.actorOf(Props(new Actor {
          def receive: Receive = {
            case res: RpcResponseEnvelope => outHandler(res)
          }
        }))
        val inHandler = context.actorOf(ConnectionHandler(project, broadcaster, delegate))

        { req => inHandler ! req }
    }

    val docs = DocJarReading.forConfig(config)
    WebServer.start(docs, preferredHttpPort.getOrElse(0), hookHandlers).onComplete {
      case Failure(ex) =>
        log.error(ex, s"Error binding http endpoint ${ex.getMessage}")
        selfRef ! ShutdownRequest(s"http endpoint failed to bind ($preferredHttpPort)", isError = true)

      case Success(ch) =>
        this.channel = ch
        log.info(s"ENSIME HTTP on ${ch.localAddress()}")
        try {
          val port = ch.localAddress().asInstanceOf[InetSocketAddress].getPort()
          PortUtil.writePort(config.cacheDir, port, "http")
        } catch {
          case ex: Throwable =>
            log.error(ex, s"Error initializing http endpoint ${ex.getMessage}")
            selfRef ! ShutdownRequest(s"http endpoint failed to initialise: ${ex.getMessage}", isError = true)
        }
    }(context.system.dispatcher)

    Environment.info foreach log.info
  }

  override def preStart(): Unit = {
    try {
      initialiseChildren()
    } catch {
      case t: Throwable =>
        log.error(t, s"Error during startup - ${t.getMessage}")
        self ! ShutdownRequest(t.toString, isError = true)
    }
  }
  override def receive: Receive = {
    case req: ShutdownRequest =>
      triggerShutdown(req)
  }

  def triggerShutdown(request: ShutdownRequest): Unit = {
    Server.shutdown(context.system, channel, request)(serverConfig)
  }

}

object Server extends AkkaBackCompat {
  Slf4jSetup.init()

  val log = LoggerFactory.getLogger("Server")

  def main(args: Array[String]): Unit = {
    val ensimeFileStr = propOrNone("ensime.config").getOrElse(
      throw new RuntimeException("ensime.config (the location of the .ensime file) must be set")
    )
    val serverFileStr = propOrNone("server.config").getOrElse(
      "~/.config/ensime-server"
    )

    val ensimeFile = new File(ensimeFileStr)
    val serverFile = new File(serverFileStr)
    if (!ensimeFile.exists() || !ensimeFile.isFile)
      throw new RuntimeException(s".ensime file ($ensimeFile) not found")
    val finalServerFile = if (!serverFile.exists() || !serverFile.isFile) {
      new File("~/.ensime-server")
    } else serverFile

    implicit val serverConfig: EnsimeServerConfig = if (finalServerFile.exists() || !ensimeFile.isFile)
      EnsimeConfigProtocol.parse(Files.toString(finalServerFile, Charsets.UTF_8))
    else {
      val shutDownOnDisconnect = Option(System.getProperty("ensime.explode.on.disconnect")).isDefined
      val test = propIsSet("ensime.server.test")
      val exitAfterIndex = propOrFalse("ensime.exitAfterIndex")
      val disableSourceMonitoring = propOrFalse("ensime.disableSourceMonitoring")
      val disableClassMonitoring = propOrFalse("ensime.disableClassMonitoring")
      val sourceMode = propOrFalse("ensime.sourceMode")
      val parallelThread = Properties.propOrElse("ensime.index.parallel", "10").toInt
      val ENSIME_EXPERIMENTAL_H2 = sys.env.getOrElse("ENSIME_EXPERIMENTAL_H2", "jdbc:h2:file:")
      val protocol = propOrElse("ensime.protocol", "swank")
      val ENSIME_SKIP_JRE_INDEX = Properties.envOrNone("ENSIME_SKIP_JRE_INDEX").isDefined
      EnsimeServerConfig(shutDownOnDisconnect, test, exitAfterIndex, disableSourceMonitoring, disableClassMonitoring, sourceMode, protocol, ENSIME_EXPERIMENTAL_H2, ENSIME_SKIP_JRE_INDEX, parallelThread)
    }
    implicit val config: EnsimeConfig = try {
      EnsimeConfigProtocol.parse(Files.toString(ensimeFile, Charsets.UTF_8), Files.toString(serverFile, Charsets.UTF_8))
    } catch {
      case e: Throwable =>
        log.error(s"There was a problem parsing $ensimeFile", e)
        throw e
    }
    Canon.config = config

    val protocol: Protocol = serverConfig.protocol match {
      case "swanki" => new SwankiProtocol
      case "swank" => new SwankProtocol
      case other => throw new IllegalArgumentException(s"$other is not a valid ENSIME protocol")
    }

    val system = ActorSystem("ENSIME")
    system.actorOf(Props(new ServerActor(config, serverConfig, protocol)), "ensime-main")
  }

  def shutdown(system: ActorSystem, channel: Channel, request: ShutdownRequest)(implicit serverConf: EnsimeServerConfig): Unit = {
    val t = new Thread(new Runnable {
      def run(): Unit = {
        if (request.isError)
          log.error(s"Shutdown requested due to internal error: ${request.reason}")
        else
          log.info(s"Shutdown requested: ${request.reason}")

        log.info("Shutting down the ActorSystem")
        Try(system.terminate())

        log.info("Awaiting actor system termination")
        Try(Await.result(system.whenTerminated, Duration.Inf))

        log.info("Shutting down the Netty channel")
        Try(channel.close().sync())

        log.info("Shutdown complete")
        if (!serverConf.test) {
          if (request.isError)
            System.exit(1)
          else
            System.exit(0)
        }
      }
    })
    t.start()
  }
}
