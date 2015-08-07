package org.ensime.core

import akka.actor._
import akka.event.LoggingReceive
import akka.event.LoggingReceive.withLabel
import java.io.File
import java.nio.charset.Charset
import org.ensime.api._
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import org.ensime.model._
import org.ensime.util._
import org.slf4j.LoggerFactory
import pimpathon.file._
import scala.reflect.internal.util.{ OffsetPosition, RangePosition, SourceFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.util.Try

case class CompilerFatalError(e: Throwable)

/**
 * Information necessary to create a javadoc or scaladoc URI for a
 * particular type or type member.
 */
case class DocFqn(pack: String, typeName: String) {
  def mkString: String = if (pack.isEmpty) typeName else pack + "." + typeName
  def inPackage(prefix: String): Boolean = pack == prefix || pack.startsWith(prefix + ".")
  def javaStdLib: Boolean = inPackage("java") || inPackage("javax")
  def scalaStdLib: Boolean = inPackage("scala")
}
case class DocSig(fqn: DocFqn, member: Option[String])

/**
 * We generate DocSigs for java and scala at the same time, since we
 * don't know a priori whether the docs will be in scaladoc or javadoc
 * format.
 */
case class DocSigPair(scala: DocSig, java: DocSig)

case class DocUriReq(sig: DocSigPair)

class Analyzer(
    broadcaster: ActorRef,
    indexer: ActorRef,
    search: SearchService,
    implicit val config: EnsimeConfig,
    implicit val vfs: EnsimeVFS
) extends Actor with Stash with ActorLogging with RefactoringHandler {

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
    config.allJars.find(_.getName.contains("scala-library")) match {
      case Some(scalaLib) => settings.bootclasspath.value = scalaLib.getAbsolutePath
      case None => log.warning("scala-library.jar was not present")
    }
    settings.classpath.value = config.compileClasspath.mkString(File.pathSeparator)
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
    if (config.sourceMode) scalaCompiler.askReloadAllFiles()
  }

  protected def makeScalaCompiler() = new RichPresentationCompiler(
    config, settings, reporter, self, indexer, search
  )

  protected def restartCompiler(keepLoaded: Boolean): Unit = {
    log.warning("Restarting the Presentation Compiler")
    val files = scalaCompiler.loadedFiles
    scalaCompiler.askShutdown()
    scalaCompiler = makeScalaCompiler()
    if (keepLoaded) {
      scalaCompiler.askReloadFiles(files)
    }
    scalaCompiler.askNotifyWhenReady()
    broadcaster ! CompilerRestartedEvent

    context.become(loading orElse stashing)
  }

  override def postStop(): Unit = {
    Try(scalaCompiler.askClearTypeCache())
    Try(scalaCompiler.askShutdown())
  }

  def charset: Charset = scalaCompiler.charset

  def receive: Receive = startup orElse stashing

  def stashing: Receive = withLabel("stashing") {
    case other => stash()
  }

  def startup: Receive = withLabel("loading") {
    case FullTypeCheckCompleteEvent =>
      reporter.enable()
      // legacy clients expect to see AnalyzerReady and a
      // FullTypeCheckCompleteEvent on connection.
      broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
      broadcaster ! Broadcaster.Persist(FullTypeCheckCompleteEvent)
      context.become(ready)
      unstashAll()
  }

  // TODO: custom queue that prioritises and de-dupes restarts of the compiler
  def loading: Receive = withLabel("loading") {
    case FullTypeCheckCompleteEvent =>
      broadcaster ! FullTypeCheckCompleteEvent
      context.become(ready)
      unstashAll()
  }

  def ready: Receive = withLabel("ready") {
    case ReloadExistingFilesEvent if allFilesMode =>
      log.info("Skipping reload, in all-files mode")
    case ReloadExistingFilesEvent =>
      restartCompiler(keepLoaded = true)

    case FullTypeCheckCompleteEvent =>
      broadcaster ! FullTypeCheckCompleteEvent

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
      scalaCompiler.askRemoveDeleted(file)
      sender ! VoidResponse
    case TypecheckAllReq =>
      allFilesMode = true
      scalaCompiler.askRemoveAllDeleted()
      scalaCompiler.askReloadAllFiles()
      scalaCompiler.askNotifyWhenReady()
      sender ! VoidResponse
      context.become(loading orElse stashing)

    case UnloadAllReq =>
      if (config.sourceMode) {
        log.info("in source mode, will reload all files")
        scalaCompiler.askRemoveAllDeleted()
        restartCompiler(keepLoaded = true)
      } else {
        allFilesMode = false
        restartCompiler(keepLoaded = false)
      }
      sender ! VoidResponse
    case TypecheckFileReq(fileInfo) =>
      handleReloadFiles(List(fileInfo))
      sender ! VoidResponse

      // has the effect of waiting until the presentation compiler has
      // typechecked the file before responding to any more questions.
      context.become(loading orElse stashing)

    case TypecheckFilesReq(files) =>
      handleReloadFiles(files.map(SourceFileInfo(_)))
      sender ! VoidResponse

    // note that we're not protecting the presentation compiler from
    // queries here (it will be doing work in the background to
    // typecheck these files), so we're relying on askLoadedTyped
    // calls to ensure consistency. This is a performance
    // optimisation and it may be worth investigating what the
    // tradeoffs of `become` are in typical usecases.

    case req: PrepareRefactorReq =>
      handleRefactorPrepareRequest(req)
    case req: ExecRefactorReq =>
      handleRefactorExec(req)
    case req: CancelRefactorReq =>
      handleRefactorCancel(req)
    case CompletionsReq(fileInfo, point, maxResults, caseSens, reload) =>
      val sourcefile = createSourceFile(fileInfo)
      reporter.disable()
      val p = new OffsetPosition(sourcefile, point)
      val info = scalaCompiler.askCompletionsAt(p, maxResults, caseSens)
      sender ! info
    case UsesOfSymbolAtPointReq(file, point) =>
      val p = pos(file, point)
      val uses = scalaCompiler.askUsesOfSymAtPoint(p)
      sender ! ERangePositions(uses.map(ERangePositionHelper.fromRangePosition))
    case PackageMemberCompletionReq(path: String, prefix: String) =>
      val members = scalaCompiler.askCompletePackageMember(path, prefix)
      sender ! members
    case InspectTypeAtPointReq(file: File, range: OffsetRange) =>
      val p = pos(file, range)
      sender ! scalaCompiler.askInspectTypeAt(p)
    case InspectTypeByIdReq(id: Int) =>
      sender ! scalaCompiler.askInspectTypeById(id)
    case InspectTypeByNameReq(name: String) =>
      sender ! scalaCompiler.askInspectTypeByName(name)
    case SymbolAtPointReq(file: File, point: Int) =>
      val p = pos(file, point)
      sender ! scalaCompiler.askSymbolInfoAt(p)
    case SymbolByNameReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) =>
      sender ! scalaCompiler.askSymbolByName(typeFullName, memberName, signatureString)

    case DocUriAtPointReq(file: File, range: OffsetRange) =>
      val p = pos(file, range)
      sender() ! scalaCompiler.askDocSignatureAtPoint(p)
    case DocUriForSymbolReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) =>
      sender() ! scalaCompiler.askDocSignatureForSymbol(typeFullName, memberName, signatureString)

    case InspectPackageByPathReq(path: String) =>
      sender ! scalaCompiler.askPackageByPath(path)
    case TypeAtPointReq(file: File, range: OffsetRange) =>
      val p = pos(file, range)
      sender ! scalaCompiler.askTypeInfoAt(p)
    case TypeByIdReq(id: Int) =>
      sender ! scalaCompiler.askTypeInfoById(id)
    case TypeByNameReq(name: String) =>
      sender ! scalaCompiler.askTypeInfoByName(name)
    case TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) =>
      val p = pos(file, range)
      sender ! scalaCompiler.askTypeInfoByNameAt(name, p)
    case CallCompletionReq(id: Int) =>
      sender ! scalaCompiler.askCallCompletionInfoById(id)
    case SymbolDesignationsReq(f, start, end, tpes) =>
      if (!FileUtils.isScalaSourceFile(f)) {
        sender ! SymbolDesignations(f, List.empty)
      } else {
        val sf = createSourceFile(f)
        val clampedEnd = math.max(end, start)
        val pos = new RangePosition(sf, start, start, clampedEnd)
        if (tpes.nonEmpty) {
          val syms = scalaCompiler.askSymbolDesignationsInRegion(pos, tpes)
          sender ! syms
        } else {
          sender ! SymbolDesignations(file(sf.path), List.empty)
        }
      }

    case ImplicitInfoReq(file: File, range: OffsetRange) =>
      val p = pos(file, range)
      sender() ! scalaCompiler.askImplicitInfoInRegion(p)

    case ExpandSelectionReq(file, start: Int, stop: Int) =>
      sender ! handleExpandselection(file, start, stop)
    case FormatSourceReq(files: List[File]) =>
      handleFormatFiles(files)
      sender ! VoidResponse
    case FormatOneSourceReq(fileInfo: SourceFileInfo) =>
      sender ! StringResponse(handleFormatFile(fileInfo))

  }

  def handleReloadFiles(files: List[SourceFileInfo]): Unit = {
    files foreach { file =>
      require(file.file.exists, "" + file + " does not exist")
    }

    val (javas, scalas) = files.filter(_.file.exists).partition(
      _.file.getName.endsWith(".java")
    )

    if (scalas.nonEmpty) {
      val sourceFiles = scalas.map(createSourceFile)
      scalaCompiler.askReloadFiles(sourceFiles)
      scalaCompiler.askNotifyWhenReady()
    }
  }

  def pos(file: File, range: OffsetRange): OffsetPosition = {
    val f = scalaCompiler.createSourceFile(file.canon.getPath)
    if (range.from == range.to) new OffsetPosition(f, range.from)
    else new RangePosition(f, range.from, range.from, range.to)
  }

  def pos(file: File, offset: Int): OffsetPosition = {
    val f = scalaCompiler.createSourceFile(file.canon.getPath)
    new OffsetPosition(f, offset)
  }

  def createSourceFile(file: File): SourceFile = {
    scalaCompiler.createSourceFile(file.canon.getPath)
  }

  def createSourceFile(file: SourceFileInfo): SourceFile = {
    scalaCompiler.createSourceFile(file)
  }

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
  ) = Props(classOf[Analyzer], broadcaster, indexer, search, config, vfs)
}
