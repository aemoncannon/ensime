package org.ensime.core

import akka.actor._
import akka.event.LoggingReceive.withLabel
import org.ensime.api._
import org.ensime.config.RichEnsimeModule
import org.ensime.util.FileUtils.toSourceFileInfo
import org.ensime.util.ensimefile._
import org.ensime.util.file._

import scala.collection.breakOut

trait ModuleFinder {
  implicit val config: EnsimeConfig

  def getModule(path: String): Option[EnsimeProject] =
    config.projects.find(_.sources.exists(f => path.startsWith(f.toString)))

  def getModule(file: EnsimeFile): Option[EnsimeProject] = file match {
    case RawFile(file) => getModule(file.toString)
    case archive: ArchiveFile => getModule(archive.fullPath)
  }
  def getModule(file: File): Option[EnsimeProject] = getModule(file.toPath.toString)
  def getModule(file: Either[File, SourceFileInfo]): Option[EnsimeProject] = file match {
    case Left(f) => getModule(f)
    case Right(sourceFileInfo) => getModule(sourceFileInfo.file)
  }
}

class AnalyzerManager(
    broadcaster: ActorRef,
    analyzerCreator: List[EnsimeProject] => Props,
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging with ModuleFinder with Stash {

  // for legacy requests, the all-seeing analyzer
  private var sauron: ActorRef = _
  // FIXME = analyzerCreator(config.projects)
3
  // maps the active modules to their analyzers
  private var analyzers: Map[EnsimeProject, ActorRef] = Map.empty

  // FIXME: should we perhaps have child actors to manage the state?
  //        It is very awkward to have to update this all the time.
  private var userState: Map[EnsimeProject, List[SourceFileInfo]] = Map.empty

  override def preStart(): Unit = {
    config.projects foreach (p => historyOfModule += (p -> LoadedFilesData(Map.empty, NotLoaded)))

    broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
    broadcaster ! Broadcaster.Persist(FullTypeCheckCompleteEvent)
  }

  override def receive: Receive = ready

  private def ready: Receive = withLabel("ready") {
    case ReloadExistingFilesEvent =>
      if (analyzers.isEmpty)
        broadcaster ! AnalyzerReadyEvent
      else
        for {
          (module, analyzer) <- analyzers
        } analyzer forward ReloadExistingFilesEvent
    case req: RpcAnalyserRequest =>
      forwardToAnalyzer(req)
  }

  private def getOrSpawnNew(module: EnsimeProject): ActorRef =
    analyzers.get(module) match {
      case Some(analyzer) => analyzer
      case None =>
        // spawn a new analyzer and attach it to module
        val newAnalyzer = context.actorOf(analyzerCreator(module))
        analyzers = analyzers + (module -> newAnalyzer)
        newAnalyzer
    }

  def toFile(f: SourceFileInfo) = f.file match {
    case RawFile(path) => path.toFile
    case file: ArchiveFile => File(file.fullPath) // not sure
  }

  def update(loadedFilesData: LoadedFilesData)(f: (Map[SourceFileInfo, FileStatus], FileStatus) => LoadedFilesData) =
    f(loadedFilesData.statusOfFile, loadedFilesData.default)

  def allSourceFiles(module: EnsimeProject): Set[SourceFileInfo] =
    module.scalaSourceFiles.map(f => SourceFileInfo(RawFile(f.toPath), None, None))(breakOut)

  def removeSymbolsOf(module: EnsimeProject): Set[SourceFileInfo] = {
    val ld = historyOfModule(module)
    allSourceFiles(module) filter (ld.statusOfFile.getOrElse(_, ld.default) == Removed)
  }

  private def withExistingModuleFor(fileInfo: SourceFileInfo, req: RpcAnalyserRequest)(f: (RpcAnalyserRequest, EnsimeProject) => Unit): Unit =
    getModule(fileInfo.file) match {
      case Some(module) =>
        f(req, module)
      case None =>
        sender ! EnsimeServerError("Update .ensime file.")
    }

  private def withExistingModule(moduleId: EnsimeProjectId, req: RpcAnalyserRequest)(f: (RpcAnalyserRequest, EnsimeProject) => Unit) =
    config.projects.find(_.id == moduleId) match {
      case Some(module) =>
        f(req, module)
      case None =>
        sender ! EnsimeServerError("Update .ensime file.")
    }

  private def forwardToAnalyzer: PartialFunction[RpcAnalyserRequest, Unit] = {
    case req @ TypecheckAllReq =>
      config.projects.foreach { module =>
        getOrSpawnNew(module) forward req
        historyOfModule += module -> LoadedFilesData(Map.empty, Loaded)
      }
    case req @ UnloadAllReq =>
      analyzers.foreach {
        case (_, analyzer) => analyzer forward req
      }
    case req @ TypecheckModule(moduleId) =>
      withExistingModule(moduleId, req)((req, module) => {
        getOrSpawnNew(module) forward req
        historyOfModule += module -> LoadedFilesData(Map.empty, Loaded)
      })
    case req @ UnloadModuleReq(moduleId) =>
      withExistingModule(moduleId, req)((req, module) => {
        getOrSpawnNew(module) forward req
        val previousData = historyOfModule(module)
        historyOfModule += module -> update(previousData) { (statusOfFile, default) =>
          default match {
            case NotLoaded =>
              LoadedFilesData(statusOfFile.map(m => m._1 -> m._2.append(Removed)), default)
            case _ =>
              LoadedFilesData(Map.empty, Removed)
          }
        }
      })
    case req @ RemoveFileReq(file: File) =>
      val f = SourceFileInfo(RawFile(file.toPath), None, None)
      withExistingModuleFor(f, req)((req, module) => {
        getOrSpawnNew(module) forward req
        val previousData = historyOfModule(module)
        historyOfModule += module -> update(previousData) { (statusOfFile, default) =>
          LoadedFilesData(statusOfFile + (f -> statusOfFile.getOrElse(f, default).append(Removed)), default)
        }
      })
    case req @ TypecheckFileReq(fileInfo) =>
      withExistingModuleFor(fileInfo, req)((req, module) => {
        getOrSpawnNew(module) forward req
        val previousData = historyOfModule(module)
        historyOfModule += module -> update(previousData) { (statusOfFile, default) =>
          LoadedFilesData(statusOfFile + (fileInfo -> statusOfFile.getOrElse(fileInfo, default).append(Loaded)), default)
        }
      })
    case req @ TypecheckFilesReq(files) =>
      var missingSource: Boolean = files.exists(getModule(_).isEmpty)
      if (missingSource)
        sender ! EnsimeServerError("Update .ensime file.")
      else {
        val filesPerModule: Map[EnsimeProject, List[Either[File, SourceFileInfo]]] = files.groupBy(getModule).map(x => x._1.get -> x._2)
        for ((module, list) <- filesPerModule) {
          val analyzer = getOrSpawnNew(module)
          analyzer ! TypecheckFilesReq(list)
          if (!list.map(toSourceFileInfo).exists(!_.file.exists)) {
            val previousData = historyOfModule(module)
            historyOfModule += module -> update(previousData) { (statusOfFile, default) =>
              val addToStatusOfFile = list.map(toSourceFileInfo).map(f => f -> statusOfFile.getOrElse(f, default).append(Loaded))
              LoadedFilesData(statusOfFile ++ addToStatusOfFile, default)
            }
          }

        }
        // FIXME: argh, now we block all other messages so we can only serve one analyzer at a time
        context.become(collector[List[String]](filesPerModule.size, Nil, sender)((newResponse, aggregate) => {
          newResponse match {
            case EnsimeServerError(desc) =>
              desc :: aggregate
            case _ =>
              aggregate
          }
        }, aggregate => {
          if (aggregate.isEmpty) // had no errors; return a  VoidResponse
            VoidResponse
          else // return the cumulative error
            EnsimeServerError(aggregate mkString ", ")
        }))
      }
    case req @ (InspectTypeByNameReq(_) |
      SymbolByNameReq(_, _, _) |
      DocUriForSymbolReq(_, _, _) |
      PackageMemberCompletionReq(_, _) |
      TypeByNameReq(_) |
      InspectPackageByPathReq(_)) =>
      val filesToBeDeleted = config.projects.flatMap(removeSymbolsOf(_)).filter(_.file.exists())
      val originalSender = sender
      context.actorOf(Props(new Actor {
        private var analyzer: ActorRef = _
        override def preStart(): Unit = {
          analyzer = context.actorOf(projectWideAnalyzer)
          if (filesToBeDeleted.nonEmpty)
            analyzer ! TypecheckFilesReq(filesToBeDeleted.map(Right(_)).toList)
          else
            self ! FullTypeCheckCompleteEvent
        }
        def receive: Receive = {
          case FullTypeCheckCompleteEvent =>
            analyzer ! RemoveFilesReq(filesToBeDeleted.map(toFile))
            analyzer ! req
          case VoidResponse => // filter out
          case res =>
            originalSender ! res
            context.stop(analyzer)
            context.stop(self)
        }
      }))
    case req @ RefactorReq(_, _, _) =>
      context.actorOf(projectWideAnalyzer) forward req
    case req @ CompletionsReq(fileInfo, _, _, _, _) =>
      withExistingModuleFor(fileInfo, req)((req, module) =>
        getOrSpawnNew(module) forward req)
    case req @ UsesOfSymbolAtPointReq(f, _) =>
      withExistingModuleFor(f, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ InspectTypeAtPointReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ SymbolAtPointReq(file, point: Int) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ DocUriAtPointReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ TypeAtPointReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ TypeByNameAtPointReq(name: String, file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ SymbolDesignationsReq(f, start, end, _) =>
      withExistingModuleFor(f, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ ImplicitInfoReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ ExpandSelectionReq(file, start: Int, stop: Int) =>
      val f = SourceFileInfo(RawFile(file.toPath), None, None)
      withExistingModuleFor(f, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ StructureViewReq(fileInfo: SourceFileInfo) =>
      withExistingModuleFor(fileInfo, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ AstAtPointReq(file, offset) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
      })
    case req @ UnloadFileReq(file) =>
      withExistingModuleFor(file, req)((req, module) => {
        getOrSpawnNew(module) forward req
        val previousData = historyOfModule(module)
        historyOfModule += module -> update(previousData) { (statusOfFile, default) =>
          LoadedFilesData(statusOfFile + (file -> statusOfFile.getOrElse(file, default).append(Unloaded)), default)
        }
      })
  }

  /**
   * T is the type of parameter we need to agregate, for e.g, List[String], TypeInfo, PackageInfo etc.
   *
   *  @param remaining      number of responses that still need to be collected
   *  @param aggregate      the agg
   *  @param sendResultsTo  the original sender
   *  @param addResponse    the aggregating function; takes as input the last aggregate and new response and, returns the new aggregate
   *  @param combine        the combining function; makes the response from the final aggregate
   *
   * for instance, for collecting results of a TypeCheckFilesReq we will define it as follows :
   *
   *  collector[List[String], RpcResponse](remaining, Nil, sender)(...)
   *
   */
  private def collector[T](remaining: Int, aggregate: T, sendResultsTo: ActorRef)(addResponse: (RpcResponse, T) => T, combine: T => RpcResponse): Receive =
    if (remaining > 1) {
      case res: RpcResponse =>
        context.become(collector(remaining - 1, addResponse(res, aggregate), sendResultsTo)(addResponse, combine))
      case msg => stash()
    } else {
      case res: RpcResponse =>
        sendResultsTo ! combine(addResponse(res, aggregate))
        context.become(ready)
        unstashAll()
      case msg => stash()
    }

}

object AnalyzerManager {
  def apply(
    broadcaster: ActorRef,
    creator: EnsimeProject => Props
  )(
    implicit
    config: EnsimeConfig
  ) = Props(new AnalyzerManager(broadcaster, creator, config))
}
