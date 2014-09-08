package org.ensime.server

import java.io._
import java.net.{ InetAddress, ServerSocket, Socket }

import akka.actor._
import org.ensime.config.{ EnsimeConfig, Environment, ProjectConfig }
import org.ensime.protocol._
import org.ensime.util.{ SExpParser, WireFormat, _ }
import org.slf4j._
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.io.Source
import scala.util.Properties
import scala.util.Properties._

object Server {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  val log = LoggerFactory.getLogger(classOf[Server])

  def main(args: Array[String]): Unit = {
    val ensimeFileStr = propOrNone("ensime.config").getOrElse(
      throw new RuntimeException("ensime.config (the location of the .ensime file) must be set"))

    val ensimeFile = new File(ensimeFileStr)
    if (!ensimeFile.exists() || !ensimeFile.isFile)
      throw new RuntimeException(s".ensime file ($ensimeFile) not found")

    val cacheDirStr = propOrNone("ensime.cachedir").getOrElse(
      throw new RuntimeException("ensime.cachedir must be set"))

    val cacheDir = new File(cacheDirStr)

    val rootDir = ensimeFile.getParentFile

    val activeModule = propOrNone("ensime.active").getOrElse(
      throw new RuntimeException("ensime.active must be set"))

    initialiseServer(ensimeFile, activeModule, rootDir, cacheDir)
  }

  def initialiseServer(ensimeFile: File, activeModule: String, rootDir: File, cacheDir: File): Server = {
    val oldConfig = readOldEnsimeConfig(ensimeFile, activeModule)
    val newConfig = readEnsimeConfig(ensimeFile, activeModule, rootDir, cacheDir)
    val server = new Server(newConfig, oldConfig, cacheDir, "127.0.0.1", 0)
    server.start()
    //    println("HERE")
    server
  }

  /**
   * ******************************************************************************
   * Read a new style config from the given files.
   * @param ensimeFile The base ensime file.
   * @param activeModule The active module
   * @param rootDir The project root directory
   * @param cacheDir The
   * @return
   */
  def readEnsimeConfig(ensimeFile: File, activeModule: String, rootDir: File, cacheDir: File): EnsimeConfig = {
    // TODO Update this to currently parse the file
    val configSrc = Source.fromFile(ensimeFile)
    try {
      val content = configSrc.getLines().filterNot(_.startsWith(";;")).mkString("\n")
      val parsed = SExpParser.read(content).asInstanceOf[SExpList]

      new EnsimeConfig(ensimeFile, rootDir, cacheDir, parsed)
    } finally {
      configSrc.close()
    }
  }
  /**
   * ******************************************************************************
   * Read the .ensime config old style (add the active module as this is what
   * is appended in the config file by emacs.  We manually add it to the config
   * @param ensimeFile The .ensime file location
   * @param activeModule the active module
   * @return An old style ProjectConfig
   */
  def readOldEnsimeConfig(ensimeFile: File, activeModule: String): ProjectConfig = {

    val configSrc = Source.fromFile(ensimeFile)
    try {
      val content = configSrc.getLines().filterNot(_.startsWith(";;")).mkString("\n")
      val parsed = SExpParser.read(content).asInstanceOf[SExpList]
      val withActiveProject = SExpList(parsed.items ::: List(KeywordAtom(":active-subproject"), StringAtom(activeModule)))

      ProjectConfig.fromSExp(withActiveProject) match {
        case Right(config) =>
          config
        case Left(ex) =>
          throw ex
      }
    } finally {
      configSrc.close()
    }
  }
}

class Server(ensimeConfig: EnsimeConfig, projectConfig: ProjectConfig, cacheDir: File, host: String, requestedPort: Int) {

  import org.ensime.server.Server.log

  require(!cacheDir.exists || cacheDir.isDirectory, cacheDir + " is not a valid cache directory")
  cacheDir.mkdirs()

  val actorSystem = ActorSystem.create()
  // TODO move this to only be started when we want to receive
  val listener = new ServerSocket(requestedPort, 0, InetAddress.getByName(host))
  val actualPort = listener.getLocalPort

  log.info("ENSIME Server on " + host + ":" + actualPort)
  log.info("cacheDir=" + cacheDir)
  log.info(Environment.info)

  writePort(cacheDir, actualPort)

  val protocol = new SwankProtocol
  val project = new Project(projectConfig, cacheDir, protocol, actorSystem)

  def start() {
    project.initProject()
    startSocketListener()
  }

  def startSocketListener(): Unit = {
    val t = new Thread(new Runnable() {
      def run() {
        try {
          while (true) {
            try {
              val socket = listener.accept()
              log.info("Got connection, creating handler...")
              actorSystem.actorOf(Props(classOf[SocketHandler], socket, protocol, project))
            } catch {
              case e: IOException =>
                log.error("ENSIME Server: ", e)
            }
          }
        } finally {
          listener.close()
        }
      }
    })
    t.start()
  }

  def shutdown() {
    log.info("Shutting down server")
    listener.close()
    actorSystem.shutdown()
  }
  private def writePort(cacheDir: File, port: Int): Unit = {
    val portfile = new File(cacheDir, "port")
    if (!portfile.exists()) {
      log.info("Creating portfile " + portfile)
      log.info("creating portfile " + portfile)
      portfile.createNewFile()
    } else if (portfile.length > 0)
      // LEGACY: older clients create an empty file
      throw new IOException(
        "An ENSIME server might be open already for this project. " +
          "If you are sure this is not the case, please delete " +
          portfile.getAbsolutePath + " and try again"
      )

    portfile.deleteOnExit()
    val out = new PrintWriter(portfile)
    try out.println(port)
    finally out.close()
  }
}

case object SocketClosed

class SocketReader(socket: Socket, protocol: Protocol, handler: ActorRef) extends Thread {
  val log = LoggerFactory.getLogger(this.getClass)
  val in = new BufferedInputStream(socket.getInputStream)

  override def run() {
    try {
      while (true) {
        val msg: WireFormat = protocol.readMessage(in)
        handler ! IncomingMessageEvent(msg)
      }
    } catch {
      case e: IOException =>
        log.error("Error in socket reader: ", e)
        Properties.envOrNone("ensime.explode.on.disconnect") match {
          case Some(_) =>
            log.warn("tick, tick, tick, tick... boom!")
            System.exit(-1)
          case None =>
            handler ! SocketClosed
        }
    }
  }
}

class SocketHandler(socket: Socket, protocol: Protocol, project: Project) extends Actor with ActorLogging {
  protocol.setOutputActor(self)

  val reader = new SocketReader(socket, protocol, self)
  val out = new BufferedOutputStream(socket.getOutputStream)

  def write(value: WireFormat) {
    try {
      protocol.writeMessage(value, out)
    } catch {
      case e: IOException =>
        log.error(e, "Write to client failed")
        context.stop(self)
    }
  }

  override def preStart() {
    reader.start()
  }

  override def receive = {
    case IncomingMessageEvent(value: WireFormat) =>
      project ! IncomingMessageEvent(value)
    case OutgoingMessageEvent(value: WireFormat) =>
      write(value)
    case SocketClosed =>
      log.error("Socket closed, stopping self")
      context.stop(self)
  }
}
