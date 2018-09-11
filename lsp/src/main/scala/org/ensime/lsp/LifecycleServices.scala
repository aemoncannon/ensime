// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs _ =>
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp

import java.io.File
import java.nio.charset.Charset

import akka.actor.ActorSystem
import com.typesafe.config._
import org.ensime.api._
import org.ensime.config.EnsimeConfigProtocol
import org.ensime.config.richconfig._
import org.ensime.core.{ Broadcaster, Project }
import org.ensime.util.file._
import org.ensime.util.path._
import monix.eval.Task
import io.circe.{ Json, JsonObject }
import io.circe.syntax._

import scala.meta.jsonrpc.Response
import scala.meta.lsp.{
  CompletionOptions,
  InitializeParams,
  InitializeResult,
  ServerCapabilities,
  TextDocumentSyncKind,
  TextDocumentSyncOptions
}
import scribe.Logger

/**
 * Services for the initialization and termination of the ensime state
 *
 * @param ref  A mutable reference to the [[EnsimeState]].  The state is set on
 *             initialization.  Other services may access the state, but the
 *             [[LifecycleServices]] are responsible for mutating it.
 */
final class LifecycleServices(ref: OptionalRef[EnsimeState], log: Logger) {

  /**
   * Initializes the ensime actor system using the .ensime config file.
   *
   * @param params The root path and client capabilities.  The root path is used
   *               to locate the .ensime file.  The client capabilities are
   *               currently unused, but should probably be inspected to verify
   *               that the client can handle the server's responses.
   * @return       An [[InitializeResult]] on successful initialization or a
   *              [[Response.Error]] on failure.  See
   *              [[LifecycleServices.InitializationError]]
   */
  def initialize(
    params: InitializeParams
  ): Task[Either[Response.Error, InitializeResult]] = {
    val rootFile = new File(params.rootPath)

    createCache(rootFile).flatMap { cache: EnsimeCache =>
      val ensimeFile = new File(s"${params.rootPath}/.ensime")
      loadTypesafeConfig(ensimeFile).flatMap { typesafeConfig =>
        loadEnsimeConfig(typesafeConfig).flatMap {
          case (ensimeConfig, serverConfig) =>
            createActorSystem(typesafeConfig, ensimeConfig, serverConfig)
              .flatMap[LifecycleServices.InitializationError, Unit] {
                case (system, project) =>
                  EitherTask.fromTask(
                    ref.put(EnsimeState(system, cache, project))
                  )
              }
        }
      }
    }.map { _ =>
      InitializeResult(
        ServerCapabilities(
          completionProvider = Some(CompletionOptions(false, Seq("."))),
          definitionProvider = true,
          hoverProvider = true,
          codeActionProvider = false,
          documentSymbolProvider = true,
          textDocumentSync = Some(
            TextDocumentSyncOptions(
              change = Some(TextDocumentSyncKind.Full)
            )
          )
        )
      )
    }.value.map(_.left.map(error => Response.internalError(error.message)))
  }

  /** Called when the client has received initialization parameters */
  def initialized(params: Json): Unit =
    log.info("Lsp client has confirmed initialization")

  /**
   * Shuts down the actor system
   *
   * Shutdown will fail if the system isn't initialized in the first place or
   * if the actor system fails to terminate.
   *
   * @return An empty json object on successful shutdown or a [[Response.Error]]
   */
  def shutdown: Task[Either[Response.Error, Json]] =
    EitherTask
      .fromTask(Task.eval(log.info("Received shutdown command")))
      .flatMap { _ =>
        EitherTask(
          ref.get
            .map(_.left.map(_ => LifecycleServices.EnsimeSystemUninitialized))
        )
      }
      .flatMap {
        case EnsimeState(system, _, _) =>
          system.terminate()
          log.info("Shutting down actor system.")
          EitherTask.fromTask[LifecycleServices.ShutdownError, Unit](
            Task.fromFuture(system.whenTerminated).map(_ => ())
          )
      }
      .flatMap(
        _ =>
          EitherTask.fromTask[LifecycleServices.ShutdownError, Unit](Task {
            log.info("Actor system down.")
          })
      )
      .map(_ => JsonObject.empty.asJson)
      .value
      .map(_.left.map(e => Response.internalError(e.message)))
      .onErrorHandle(t => Left(Response.internalError(t.getMessage)))

  /**
   * Creates the .ensime_cache directory.  This contains a cache of unzipped
   * source jar files.
   *
   * The .ensime_cache directory is created in the root of the project if it
   * doesn't already exist.
   *
   * @param rootFile  The root of the project that the LSP client is being used
   *                  in
   */
  private def createCache(
    rootFile: File
  ): EitherTask[LifecycleServices.FailedToCreateEnsimeCache.type, EnsimeCache] =
    EitherTask
      .fromTask(
        Task.eval {
          val cacheDir = new File(rootFile, ".ensime_cache")
          cacheDir.mkdir()
          cacheDir
        }
      )
      .flatMap(
        file =>
          EitherTask.fromEither(
            EnsimeCache
              .fromPath(file.toString)
              .left
              .map(_ => LifecycleServices.FailedToCreateEnsimeCache)
        )
      )
      .flatMapTask(identity)
      .onErrorHandle(_ => LifecycleServices.FailedToCreateEnsimeCache)

  /**
   * Loads the typesafe config file (application.conf) from the classpath.
   *
   * The "ensime.config" value is set to the .ensime file in the project root.
   */
  private def loadTypesafeConfig(
    ensimeFile: File
  ): EitherTask[LifecycleServices.FailedToLoadTypesafeConfig.type, Config] =
    EitherTask
      .fromTask(
        Task {
          val config   = s"""ensime.config = "${ensimeFile.toString}" """
          val fallback = ConfigFactory.parseString(config)
          ConfigFactory.load().withFallback(fallback)
        }
      )
      .onErrorHandle(_ => LifecycleServices.FailedToLoadTypesafeConfig)

  /**
   * Loads the ensime server config and ensime config
   *
   * The [[EnsimeServerConfig]] is loaded from typesafe config alone, while the
   * [[EnsimeConfig]] is loaded from the .ensime file.
   *
   * @param config  The typesafe config object. This is used to create the
   *                [[EnsimeserverConfig]].  It contains the location of the
   *                .ensime file used to create the [[EnsimeConfig]].
   *
   */
  private def loadEnsimeConfig(
    config: Config
  ): EitherTask[LifecycleServices.FailedToLoadEnsimeConfig.type,
                (EnsimeConfig, EnsimeServerConfig)] =
    EitherTask
      .fromTask(Task.eval {
        val serverConfig = parseServerConfig(config)
        val ensimeConfig = EnsimeConfigProtocol.parse(
          serverConfig.config.file.readString()(LifecycleServices.Utf8Charset)
        )
        (ensimeConfig, serverConfig)
      })
      .onErrorHandle(_ => LifecycleServices.FailedToLoadEnsimeConfig)

  /**
   * Creates the Ensime actor system and project actor
   *
   * @param config       The typesafe config
   * @param ensimeConfig The .ensime config of the target project
   * @param serverConfig The Ensime server config loaded from the typesafe
   *                     config
   */
  private def createActorSystem(
    config: Config,
    ensimeConfig: EnsimeConfig,
    serverConfig: EnsimeServerConfig
  ): EitherTask[LifecycleServices.FailedToCreateActorSystem,
                (ActorSystem, EnsimeProjectWrapper)] =
    EitherTask
      .fromTask(Task.eval {
        val system      = ActorSystem("ENSIME", config)
        val broadcaster = system.actorOf(Broadcaster(), "broadcaster")
        val project = EnsimeProjectWrapper(
          system.actorOf(Project(broadcaster)(ensimeConfig, serverConfig),
                         "project")
        )
        (system, project)
      })
      .onErrorHandle(t => LifecycleServices.FailedToCreateActorSystem(t))
}

object LifecycleServices {

  private[lsp] val Utf8Charset: Charset = Charset.forName("UTF-8")

  /** Represents everything that could possibly go wrong in initialization */
  sealed trait InitializationError {
    def message: String = this match {
      case FailedToCreateEnsimeCache =>
        "Failed to create .ensime_cache directory"
      case FailedToLoadTypesafeConfig => "Failed to load config file"
      case FailedToLoadEnsimeConfig   => "Failed to load .ensime config"
      case FailedToCreateActorSystem(t) =>
        s"Failed to create storage: ${t.getMessage}"
    }
  }

  case object FailedToCreateEnsimeCache  extends InitializationError
  case object FailedToLoadTypesafeConfig extends InitializationError
  case object FailedToLoadEnsimeConfig   extends InitializationError
  final case class FailedToCreateActorSystem(t: Throwable)
      extends InitializationError

  sealed trait ShutdownError {
    def message: String = this match {
      case EnsimeSystemUninitialized => Uninitialized.message
    }
  }

  case object EnsimeSystemUninitialized extends ShutdownError
}
