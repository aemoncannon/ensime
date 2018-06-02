// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.io.{ FileOutputStream, PrintStream }
import java.net.InetSocketAddress
import java.nio.file.Paths

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util._
import akka.actor._
import akka.actor.SupervisorStrategy.Stop
import com.typesafe.config._
import io.netty.channel.Channel
import org.ensime.api._
import org.ensime.config._
import org.ensime.config.richconfig._
import org.ensime.core._
import org.ensime.lsp.ensime.EnsimeLanguageServer
import org.ensime.util.Slf4jSetup
import org.ensime.api.EnsimeFile.Implicits.DefaultCharset
import org.ensime.util.path._
import org.slf4j._

class ServerActor(
  config: EnsimeConfig,
  serverConfig: EnsimeServerConfig
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
        req.fold(err => inHandler ! err, msg => inHandler ! msg)
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
  def props()(implicit
              ensimeConfig: EnsimeConfig,
              serverConfig: EnsimeServerConfig): Props =
    Props(new ServerActor(ensimeConfig, serverConfig))
}

object Server {
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

  def startRegularServer(): Unit = {
    val config                                    = loadConfig()
    implicit val serverConfig: EnsimeServerConfig = parseServerConfig(config)

    EnsimeConfigProtocol.parse(serverConfig.config.file.readString()) match {
      case Right(cfg) =>
        implicit val ensimeConfig: EnsimeConfig = cfg
        ActorSystem
          .create("ENSIME", config)
          .actorOf(ServerActor.props(), "ensime-main")
      case Left(err) => System.err.println(s"Failed to parse .ensime $err")
    }
  }

  def startLspServer(): Unit = {
    val cwd    = Option(System.getProperty("lsp.workspace")).getOrElse(".")
    val server = new EnsimeLanguageServer(System.in, System.out)

    // route System.out somewhere else. The presentation compiler may spit out text
    // and that confuses VScode, since stdout is used for the language server protocol
    val origOut = System.out
    try {
      System.setOut(
        new PrintStream(new FileOutputStream(s"$cwd/pc.stdout.log"))
      )
      System.setErr(
        new PrintStream(new FileOutputStream(s"$cwd/pc.stdout.log"))
      )
      log.info("This file contains stdout from the presentation compiler.")
      log.info(s"Starting server in $cwd")
      log.info(s"Classpath: ${Properties.javaClassPath}")
      server.start()
    } finally {
      System.setOut(origOut)
    }

    // make sure we actually exit
    System.exit(0)
  }

  def main(args: Array[String]): Unit =
    if (args.contains("--lsp")) {
      startLspServer()
    } else {
      startRegularServer()
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
