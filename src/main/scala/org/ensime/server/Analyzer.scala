package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.model.SourceFileInfo
import org.ensime.model.SymbolDesignations
import org.ensime.model.PatchOp
import org.ensime.model.OffsetRange
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.Iterable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.Settings
import scala.tools.nsc.ast._
import scala.reflect.internal.util.OffsetPosition

case class FullTypeCheckCompleteEvent()
case class CompilerFatalError(e: Throwable)

class Analyzer(
  val project: Project,
  val indexer: Actor,
  val protocol: ProtocolConversions,
  val config: ProjectConfig)
    extends Actor with RefactoringHandler {

  private val settings = new Settings(Console.println)
  println("ExtraArgs: " + config.compilerArgs)
  settings.processArguments(config.compilerArgs, false)
  settings.usejavacp.value = false

  println("\nPresentation Compiler settings:")
  println(settings.toString)
  import protocol._

  private val reportHandler = new ReportHandler {
    override def messageUser(str: String) {
      project ! AsyncEvent(
        toWF(SendBackgroundMessageEvent(
          MsgCompilerUnexpectedError, Some(str))))
    }
    override def clearAllScalaNotes() {
      project ! AsyncEvent(toWF(ClearAllNotesEvent('scala)))
    }
    override def clearAllJavaNotes() {
      project ! AsyncEvent(toWF(ClearAllNotesEvent('java)))
    }
    override def reportScalaNotes(notes: List[Note]) {
      project ! AsyncEvent(toWF(NewNotesEvent('scala, NoteList(false, notes))))
    }
    override def reportJavaNotes(notes: List[Note]) {
      project ! AsyncEvent(toWF(NewNotesEvent('java, NoteList(false, notes))))
    }
  }

  private val reporter = new PresentationReporter(reportHandler)

  protected val scalaCompiler: RichCompilerControl = new RichPresentationCompiler(
    settings, reporter, this, indexer, config)
  protected val javaCompiler: JavaCompiler = new JavaCompiler(
    config, reportHandler, indexer)
  protected var awaitingInitialCompile = true
  protected var initTime: Long = 0

  import scalaCompiler._

  def act() {
    project.bgMessage("Initializing Analyzer. Please wait...")
    initTime = System.currentTimeMillis()

    if (!config.disableSourceLoadOnStartup) {
      println("Building Java sources...")
      javaCompiler.compileAll()
      println("Building Scala sources...")
      reporter.disable()
      scalaCompiler.askReloadAllFiles()
      scalaCompiler.askNotifyWhenReady()
    } else {
      this ! FullTypeCheckCompleteEvent()
    }

    loop {
      try {
        receive {
          case AnalyzerShutdownEvent() => {
            javaCompiler.shutdown()
            scalaCompiler.askClearTypeCache()
            scalaCompiler.askShutdown()
            exit('stop)
          }

          case FullTypeCheckCompleteEvent() => {
            if (awaitingInitialCompile) {
              awaitingInitialCompile = false
              val elapsed = System.currentTimeMillis() - initTime
              println("Analyzer ready in " + elapsed / 1000.0 + " seconds.")
              reporter.enable()
              project ! AsyncEvent(toWF(AnalyzerReadyEvent()))
              indexer ! CommitReq()
            }
            project ! AsyncEvent(toWF(FullTypeCheckCompleteEvent()))
          }

          case rpcReq @ RPCRequestEvent(req: Any, callId: Int) => {
            try {
              if (awaitingInitialCompile) {
                project.sendRPCError(ErrAnalyzerNotReady,
                  Some("Analyzer is not ready! Please wait."), callId)
              } else {

                reporter.enable()

                req match {

                  case RemoveFileReq(file: File) => {
                    askRemoveDeleted(file)
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case ReloadAllReq() => {
                    javaCompiler.reset()
                    javaCompiler.compileAll()
                    scalaCompiler.askRemoveAllDeleted()
                    scalaCompiler.askReloadAllFiles()
                    scalaCompiler.askNotifyWhenReady()
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case ReloadFilesReq(files: List[SourceFileInfo]) => {
                    val (javas, scalas) = files.filter(_.file.exists).partition(
                      _.file.getName.endsWith(".java"))
                    if (!javas.isEmpty) {
                      javaCompiler.compileFiles(javas)
                    }
                    if (!scalas.isEmpty) {
                      scalaCompiler.askReloadFiles(scalas.map(createSourceFile))
                      scalaCompiler.askNotifyWhenReady()
                      project ! RPCResultEvent(toWF(true), callId)
                    }
                  }

                  case PatchSourceReq(
                    file: File, edits: List[PatchOp]) => {
                    if (!file.exists()) {
                      project.sendRPCError(ErrFileDoesNotExist,
                        Some(file.getPath()), callId)
                    } else {
                      val f = createSourceFile(file)
                      val revised = PatchSource.applyOperations(f, edits)
                      reporter.disable()
                      scalaCompiler.askReloadFile(revised)
                      project ! RPCResultEvent(toWF(true), callId)
                    }
                  }

                  case req: RefactorPerformReq => {
                    handleRefactorRequest(req, callId)
                  }

                  case req: RefactorExecReq => {
                    handleRefactorExec(req, callId)
                  }

                  case req: RefactorCancelReq => {
                    handleRefactorCancel(req, callId)
                  }

                  case CompletionsReq(file: File, point: Int,
                    maxResults: Int, caseSens: Boolean, reload: Boolean) => {
                    val p = if (reload) pos(file, point) else posNoRead(file, point)
                    reporter.disable()
                    val info = scalaCompiler.askCompletionsAt(
                      p, maxResults, caseSens)
                    project ! RPCResultEvent(toWF(info), callId)
                  }

                  case ImportSuggestionsReq(_, _, _, _) => {
                    indexer ! rpcReq
                  }

                  case PublicSymbolSearchReq(_, _) => {
                    indexer ! rpcReq
                  }

                  case UsesOfSymAtPointReq(file: File, point: Int) => {
                    val p = pos(file, point)
                    val uses = scalaCompiler.askUsesOfSymAtPoint(p)
                    project ! RPCResultEvent(toWF(uses.map(toWF)), callId)
                  }

                  case PackageMemberCompletionReq(path: String, prefix: String) => {
                    val members = scalaCompiler.askCompletePackageMember(path, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case InspectTypeReq(file: File, range: OffsetRange) => {
                    val p = pos(file, range)
                    val result = scalaCompiler.askInspectTypeAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case InspectTypeByIdReq(id: Int) => {
                    val result = scalaCompiler.askInspectTypeById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case SymbolAtPointReq(file: File, point: Int) => {
                    val p = pos(file, point)
                    val result = scalaCompiler.askSymbolInfoAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case InspectPackageByPathReq(path: String) => {
                    val result = scalaCompiler.askPackageByPath(path) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeAtPointReq(file: File, range: OffsetRange) => {
                    val p = pos(file, range)
                    val result = scalaCompiler.askTypeInfoAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByIdReq(id: Int) => {
                    val result = scalaCompiler.askTypeInfoById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByNameReq(name: String) => {
                    val result = scalaCompiler.askTypeInfoByName(name) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByNameAtPointReq(name: String, file: File,
                    range: OffsetRange) => {
                    val p = pos(file, range)
                    val result = scalaCompiler.askTypeInfoByNameAt(name, p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }

                    project ! RPCResultEvent(result, callId)
                  }

                  case CallCompletionReq(id: Int) => {
                    val result = scalaCompiler.askCallCompletionInfoById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case SymbolDesignationsReq(file: File, start: Int,
                    end: Int, tpes: List[Symbol]) => {
                    if (!FileUtils.isScalaSourceFile(file)) {
                      project ! RPCResultEvent(
                        toWF(SymbolDesignations(file.getPath, List())), callId)
                    } else {
                      val f = createSourceFile(file)
                      val clampedEnd = math.max(end, start)
                      val pos = new RangePosition(f, start, start, clampedEnd)
                      if (!tpes.isEmpty) {
                        val syms = scalaCompiler.askSymbolDesignationsInRegion(
                          pos, tpes)
                        project ! RPCResultEvent(toWF(syms), callId)
                      } else {
                        project ! RPCResultEvent(
                          toWF(SymbolDesignations(f.path, List())), callId)
                      }
                    }
                  }
                }
              }
            } catch {
              case e: Throwable =>
                System.err.println("Error handling RPC: " + e + " :\n" +
                  e.getStackTraceString)
                project.sendRPCError(ErrExceptionInAnalyzer,
                  Some("Error occurred in Analyzer. Check the server log."), callId)
            }
          }
          case other => {
            println("Analyzer: WTF, what's " + other)
          }
        }

      } catch {
        case e: Exception => {
          System.err.println("Error at Compiler message loop: " + e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  def pos(file: File, range: OffsetRange) = {
    val f = scalaCompiler.createSourceFile(file.getCanonicalPath())
    if (range.from == range.to) new OffsetPosition(f, range.from)
    else new RangePosition(f, range.from, range.from, range.to)
  }

  def pos(file: File, offset: Int) = {
    val f = scalaCompiler.createSourceFile(file.getCanonicalPath())
    new OffsetPosition(f, offset)
  }

  def posNoRead(file: File, offset: Int) = {
    val f = scalaCompiler.findSourceFile(file.getCanonicalPath()).get
    new OffsetPosition(f, offset)
  }

  def createSourceFile(file: File) = {
    scalaCompiler.createSourceFile(file.getCanonicalPath())
  }

  def createSourceFile(file: SourceFileInfo) = {
    scalaCompiler.createSourceFile(file)
  }

  override def finalize() {
    System.out.println("Finalizing Analyzer actor.")
  }

}

