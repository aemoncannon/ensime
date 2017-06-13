// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.{ File => JFile }
import java.nio.charset.Charset

import scala.collection.breakOut
import scala.concurrent.Future
import scala.reflect.internal.util.{ OffsetPosition, RangePosition, SourceFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.util.Try

import akka.actor._
import akka.event.LoggingReceive.withLabel
import akka.pattern.pipe
import org.ensime.api._
import org.ensime.config.richconfig._
import org.ensime.indexer.SearchService
import org.ensime.model._
import org.ensime.util.{ FileUtils, PresentationReporter, ReportHandler }
import org.ensime.util.file._
import org.ensime.util.sourcefile._
import org.ensime.vfs._
import org.slf4j.LoggerFactory

final case class CompilerFatalError(e: Throwable)

/**
 * Information necessary to create a javadoc or scaladoc URI for a
 * particular type or type member.
 */
final case class DocFqn(pack: String, typeName: String) {
  def mkString: String = if (pack.isEmpty) typeName else pack + "." + typeName
  def inPackage(prefix: String): Boolean = pack == prefix || pack.startsWith(prefix + ".")
  def javaStdLib: Boolean = inPackage("java") || inPackage("javax")
  def androidStdLib: Boolean = inPackage("android")
  def scalaStdLib: Boolean = inPackage("scala")
}
final case class DocSig(fqn: DocFqn, member: Option[String])

/**
 * We generate DocSigs for java and scala at the same time, since we
 * don't know a priori whether the docs will be in scaladoc or javadoc
 * format.
 */
final case class DocSigPair(scala: DocSig, java: DocSig)

class Analyzer(
    broadcaster: ActorRef,
    indexer: ActorRef,
    search: SearchService,
    implicit val config: EnsimeConfig,
    implicit val vfs: EnsimeVFS
) extends Actor with Stash with ActorLogging with RefactoringHandler {

  import context.dispatcher
  import FileUtils._

  private var allFilesMode = false

  private var settings: Settings = _
  private var reporter: PresentationReporter = _

  protected var scalaCompiler: RichCompilerControl = _

  override def preStart(): Unit = {
    val presCompLog = LoggerFactory.getLogger(classOf[Global])

    settings = new Settings(presCompLog.error)
    settings.YpresentationDebug.value = presCompLog.isTraceEnabled
    settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
    settings.verbose.value = presCompLog.isDebugEnabled
    settings.usejavacp.value = false
    config.scalaLibrary match {
      case Some(scalaLib) => settings.bootclasspath.value = scalaLib.getAbsolutePath
      case None => log.warning("scala-library.jar not present, enabling Odersky mode")
    }
    settings.classpath.value = config.classpath.mkString(JFile.pathSeparator)
    settings.processArguments(config.compilerArgs, processAll = false)
    presCompLog.debug("Presentation Compiler settings:\n" + settings)

    reporter = new PresentationReporter(new ReportHandler {
      override def messageUser(str: String): Unit = {
        broadcaster ! SendBackgroundMessageEvent(str, 101)
      }
      override def clearAllScalaNotes(): Unit = {
        broadcaster ! ClearAllScalaNotesEvent
      }
      override def reportScalaNotes(notes: List[Note]): Unit = {
        broadcaster ! NewScalaNotesEvent(isFull = false, notes)
      }
    })
    reporter.disable() // until we start up

    scalaCompiler = makeScalaCompiler()

    broadcaster ! SendBackgroundMessageEvent("Initializing Analyzer. Please wait...")

    scalaCompiler.askNotifyWhenReady()
  }

  protected def makeScalaCompiler() = new RichPresentationCompiler(
    config, settings, reporter, self, indexer, search
  )

  protected def restartCompiler(
    strategy: ReloadStrategy,
    scoped: Option[EnsimeProjectId]
  ): Unit = {
    log.warning("Restarting the Presentation Compiler")
    val files: List[SourceFile] = strategy match {
      case ReloadStrategy.UnloadAll => Nil
      case ReloadStrategy.LoadProject =>
        val scope = scoped match {
          case None => config.projects
          case Some(id) => config.lookup(id) :: Nil
        }
        for {
          project <- scope
          file <- project.scalaSourceFiles
        } yield scalaCompiler.createSourceFile(file)
      case ReloadStrategy.KeepLoaded => scalaCompiler.loadedFiles
    }

    scalaCompiler.askShutdown()
    scalaCompiler = makeScalaCompiler()

    if (files.nonEmpty)
      scalaCompiler.askReloadFiles(files)

    scalaCompiler.askNotifyWhenReady()
    broadcaster ! CompilerRestartedEvent
  }

  override def postStop(): Unit = {
    Try(scalaCompiler.askShutdown())
  }

  def charset: Charset = scalaCompiler.charset

  def receive: Receive = startup

  def startup: Receive = withLabel("startup") {
    case FullTypeCheckCompleteEvent =>
      reporter.enable()
      // legacy clients expect to see AnalyzerReady and a
      // FullTypeCheckCompleteEvent on connection.
      broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
      broadcaster ! Broadcaster.Persist(FullTypeCheckCompleteEvent)
      context.become(ready)
      unstashAll()

    case other =>
      stash()
  }

  def ready: Receive = withLabel("ready") {
    case FullTypeCheckCompleteEvent =>
      broadcaster ! FullTypeCheckCompleteEvent

    case RestartScalaCompilerReq(id, strategy) =>
      restartCompiler(strategy, id)

    case UnloadAllReq =>
      restartCompiler(ReloadStrategy.UnloadAll, None)
      sender ! VoidResponse

    case TypecheckModule(id) =>
      restartCompiler(ReloadStrategy.LoadProject, Some(id))
      sender ! VoidResponse

    case req: RpcAnalyserRequest =>
      // fommil: I'm not entirely sure about the logic of
      // enabling/disabling the reporter so I am reluctant to refactor
      // this, but it would perhaps be simpler if we enable the
      // reporter when the presentation compiler is loaded, and only
      // disable it when we explicitly want it to be quiet, instead of
      // enabling on every incoming message.
      reporter.enable()
      allTheThings(req)
  }

  def allTheThings: PartialFunction[RpcAnalyserRequest, Unit] = {
    case RemoveFileReq(file: File) =>
      self forward UnloadFilesReq(List(toSourceFileInfo(Left(file))), true)
    case UnloadFileReq(source) =>
      self forward UnloadFilesReq(List(source), false)
    case UnloadFilesReq(files, remove) =>
      scalaCompiler.askUnloadFiles(files, remove)
      sender ! VoidResponse

    case TypecheckFileReq(fileInfo) =>
      self forward TypecheckFilesReq(List(Right(fileInfo)))
    case TypecheckFilesReq(files) =>
      sender ! scalaCompiler.handleReloadFiles(files.map(toSourceFileInfo)(breakOut))

    case req: RefactorReq =>
      import context.dispatcher
      pipe(handleRefactorRequest(req)) to sender
    case CompletionsReq(fileInfo, point, maxResults, caseSens, _reload) =>
      withExistingAsync(fileInfo) {
        reporter.disable()
        scalaCompiler.askCompletionsAt(pos(fileInfo, point), maxResults, caseSens)
      } pipeTo sender

    case UsesOfSymbolAtPointReq(file, point) =>
      import context.dispatcher
      val response = if (toSourceFileInfo(file).exists()) {
        val p = pos(file, point)
        val uses = scalaCompiler.askUsesOfSymAtPos(p)
        uses.map(positions => ERangePositions(positions.map(ERangePositionHelper.fromRangePosition)))
      } else Future.successful(EnsimeServerError(s"File does not exist: ${file.file}"))
      pipe(response) to sender
    case InspectTypeAtPointReq(file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askInspectTypeAt(p).getOrElse(FalseResponse)
      }
    case SymbolAtPointReq(file, point: Int) =>
      sender ! withExisting(file) {
        val p = pos(file, point)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askSymbolInfoAt(p).getOrElse(FalseResponse)
      }
    case DocUriAtPointReq(file, range: OffsetRange) =>
      val p = pos(file, range)
      scalaCompiler.askLoadedTyped(p.source)
      sender() ! scalaCompiler.askDocSignatureAtPoint(p)
    case TypeAtPointReq(file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askTypeInfoAt(p).getOrElse(FalseResponse)
      }
    case SymbolDesignationsReq(f, start, end, Nil) =>
      sender ! SymbolDesignations(f.file, List.empty)
    case SymbolDesignationsReq(f, start, end, tpes) =>
      sender ! withExisting(f) {
        val sf = createSourceFile(f)
        val clampedEnd = math.max(end, start)
        val pos = new RangePosition(sf, start, start, clampedEnd)
        scalaCompiler.askLoadedTyped(pos.source)
        scalaCompiler.askSymbolDesignationsInRegion(pos, tpes)
      }
    case ImplicitInfoReq(file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askImplicitInfoInRegion(p)
      }
    case ExpandSelectionReq(file, start: Int, stop: Int) =>
      val p = new RangePosition(createSourceFile(file), start, start, stop)
      val enclosingPos = scalaCompiler.askEnclosingTreePosition(p)
      sender ! FileRange(file.getPath, enclosingPos.start, enclosingPos.end)
    case StructureViewReq(fileInfo: SourceFileInfo) =>
      sender ! withExisting(fileInfo) {
        val sourceFile = createSourceFile(fileInfo)
        StructureView(scalaCompiler.askStructure(sourceFile))
      }
  }

  def withExisting(x: SourceFileInfo)(f: => RpcResponse): RpcResponse =
    if (x.exists()) f else EnsimeServerError(s"File does not exist: ${x.file}")

  def withExistingAsync(x: SourceFileInfo)(f: => Future[RpcResponse]): Future[RpcResponse] =
    if (x.exists()) f else Future.successful(EnsimeServerError(s"File does not exist: ${x.file}"))

  def pos(file: File, range: OffsetRange): OffsetPosition =
    pos(createSourceFile(file), range)
  def pos(file: File, offset: Int): OffsetPosition =
    pos(createSourceFile(file), offset)
  def pos(file: SourceFileInfo, range: OffsetRange): OffsetPosition =
    pos(createSourceFile(file), range)
  def pos(file: SourceFileInfo, offset: Int): OffsetPosition =
    pos(createSourceFile(file), offset)
  def pos(f: SourceFile, range: OffsetRange): OffsetPosition = {
    if (range.from == range.to) new OffsetPosition(f, range.from)
    else new RangePosition(f, range.from, range.from, range.to)
  }
  def pos(f: SourceFile, offset: Int): OffsetPosition = new OffsetPosition(f, offset)

  def createSourceFile(file: File): SourceFile =
    scalaCompiler.createSourceFile(file.getPath)

  def createSourceFile(file: SourceFileInfo): SourceFile =
    scalaCompiler.createSourceFile(file)
}

object Analyzer {
  def apply(
    broadcaster: ActorRef,
    indexer: ActorRef,
    search: SearchService
  )(
    implicit
    config: EnsimeConfig,
    vfs: EnsimeVFS
  ) = Props(new Analyzer(broadcaster, indexer, search, config, vfs))
}
