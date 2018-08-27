// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.io.File
import java.nio.charset.Charset

import akka.actor.{ ActorRef, ActorSystem, Props }
import com.typesafe.config._
import org.ensime.api._
import org.ensime.config.EnsimeConfigProtocol
import org.ensime.config.richconfig._
import org.ensime.util.file._
import org.ensime.util.path._
import monix.eval.Task

import scala.util.{ Failure, Success, Try }
import scala.meta.jsonrpc.Response
import scala.meta.lsp.InitializeParams
import scribe.Logger

object LifecycleServices {

  private[lsp] val Utf8Charset: Charset  = Charset.forName("UTF-8")

  /**
    * Initializes the ensime actor system using the .ensime config file.
    *
    * There are several things that could go wrong here.  The .ensime config file might
    * not exist.  If this is the case, we want the user to generate it with sbt.
    * We might be unable to parse the .ensime config file.
    */
  def initialize(params: InitializeParams, languageServer: EnsimeLanguageServerLsp4s, log: Logger): Task[Either[Response.Error, (ActorSystem, TempFileStore, ActorRef)]] = Task {

    val rootFile = new File(params.rootPath)

    // Create the .ensime_cache directory
    val cacheDir = new File(rootFile, ".ensime_cache")
    cacheDir.mkdir()

    // Find the .ensime file
    val ensimeFile = new File(s"${params.rootPath}/.ensime")
    if(ensimeFile.exists) {

      // Attempt to load the typesafe config file
      loadConfig(ensimeFile).flatMap { config =>

        // Attempt to load the .ensime file
        val ensimeConfigAttempt = Try {
          val serverConfig: EnsimeServerConfig = parseServerConfig(config)
          val ensimeConfig = EnsimeConfigProtocol.parse(
            serverConfig.config.file.readString()(Utf8Charset)
          )
          (ensimeConfig, serverConfig)
        }

        ensimeConfigAttempt match {
          case Failure(e) =>
            Left(Response.internalError("Failed to load .ensime config"))
          case Success((ensimeConfig, serverConfig)) =>
            log.info(s"Using configuration: $ensimeConfig")
            Right((ensimeConfig, serverConfig, config))
        }
      }.flatMap {
        case (ensimeConfig, serverConfig, config) =>
          val t = Try {
            val system = ActorSystem("ENSIME", config)
            val fileStore = new TempFileStore(ensimeConfig.cacheDir.file.toString)
            val ensimeActor = system.actorOf(
              Props(classOf[EnsimeActor], languageServer, ensimeConfig, serverConfig),
              "server"
            )
            (system, fileStore, ensimeActor)
          }

          t match {
            case Failure(e) =>
              log.error(s"initializeEnsime: ${e.getMessage}", e)
              Left(Response.internalError(s"Error creating storage: ${e.getMessage}"))
            case Success((system, fileStore, ensimeActor)) =>

              ensimeActor ! ConnectionInfoReq

              Right((system, fileStore, ensimeActor))
          }
      }
    } else {
      Left(Response.internalError("No .ensime file in directory. Run `sbt ensimeConfig` to create one."))
    }
  }


  /**
    * Shuts down the ensime lsp server
    */
  def shutdown(system: ActorSystem, log: Logger): Task[Unit] = {
    Task {
      log.info("Shutdown request")
      system.terminate()
      log.info("Shutting down actor system.")
    }.flatMap(_ => Task.fromFuture(system.whenTerminated))
      .flatMap(_ => Task {
        log.info("Actor system down.")
      })
  }

  /**
    * Loads the typesafe config file.  If not specified, the "ensime.config" value is set to the .ensime file in the project root.
    */
  private def loadConfig(ensimeFile: File): Either[Response.Error, Config] = {
    val configAttempt = Try {
      val config   = s"""ensime.config = "${ensimeFile.toString}" """
      val fallback = ConfigFactory.parseString(config)
      ConfigFactory.load().withFallback(fallback)
    }
    configAttempt match {
      case Failure(e) =>
        Left(Response.internalError("Failed to load config file"))
      case Success(config) =>
        Right(config)
    }
  }
}
