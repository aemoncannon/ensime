// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.actor._
import akka.event.LoggingReceive.withLabel
import org.ensime.api._
import org.ensime.config.richconfig._
import org.ensime.util.FileUtils.toSourceFileInfo
import org.ensime.util.file._

class AnalyzerManager(
    broadcaster: ActorRef,
    analyzerCreator: List[EnsimeProjectId] => Props,
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging with Stash {

  // maps the active modules to their analyzers
  private var analyzers: Map[EnsimeProjectId, ActorRef] = Map.empty

  private def getOrSpawnNew(id: EnsimeProjectId): ActorRef =
    analyzers.get(id) match {
      case Some(analyzer) => analyzer
      case None =>
        val name = s"${id.project}_${id.config}"
        val newAnalyzer = context.actorOf(analyzerCreator(id :: Nil), name)
        analyzers += (id -> newAnalyzer)
        newAnalyzer
    }

  override def preStart(): Unit = {
    // for legacy clients on startup
    broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
    broadcaster ! Broadcaster.Persist(FullTypeCheckCompleteEvent)
  }

  // I'm not convinced we need the borrow pattern here, it
  // seems to introduce as much boilerplate as it removes
  // --> keeps the error handling logic in one place
  private def withExistingModuleFor(
    fileInfo: SourceFileInfo, req: RpcAnalyserRequest
  )(f: (RpcAnalyserRequest, EnsimeProjectId) => Unit): Unit =
    config.findProject(fileInfo) match {
      case Some(moduleId) =>
        f(req, moduleId)
      case None =>
        sender ! EnsimeServerError(s"Couldn't find the project for ${fileInfo.file}")
    }

  override def receive: Receive = ready

  private def ready: Receive = withLabel("ready") {
    case req @ RestartScalaCompilerReq(id, _) =>
      id match {
        case Some(projectId) =>
          analyzers.get(projectId).foreach(_ forward req)
        case None if analyzers.nonEmpty =>
          analyzers.values foreach (_ forward req)
        case None =>
          broadcaster ! AnalyzerReadyEvent
      }
    case req @ UnloadAllReq =>
      analyzers.foreach {
        case (_, analyzer) => analyzer forward req
      }
    case req @ TypecheckModule(moduleId) =>
      getOrSpawnNew(moduleId) forward req
    case req @ RemoveFileReq(file: File) =>
      val f = SourceFileInfo(RawFile(file.toPath), None, None)
      withExistingModuleFor(f, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ TypecheckFileReq(fileInfo) =>
      withExistingModuleFor(fileInfo, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ TypecheckFilesReq(files) =>
      if (files.exists(config.findProject(_).isEmpty))
        sender ! EnsimeServerError("Update .ensime file.")
      else {
        val original = sender
        val filesPerProject = files.groupBy(config.findProject(_)).map(x => x._1.get -> x._2)

        context.actorOf(Props(new Actor {
          private var remaining = filesPerProject.size
          private var aggregate: List[String] = List.empty

          override def preStart: Unit =
            for ((moduleId, list) <- filesPerProject)
              getOrSpawnNew(moduleId) ! TypecheckFilesReq(list)

          override def receive = {
            case res: RpcResponse if remaining > 1 =>
              aggregate = addResponse(res, aggregate)
              remaining -= 1
            case res: RpcResponse =>
              aggregate = addResponse(res, aggregate)
              original ! combine(aggregate)
              context.stop(self)
          }

          def addResponse(res: RpcResponse, agg: List[String]) = res match {
            case EnsimeServerError(desc) =>
              desc :: aggregate
            case _ =>
              aggregate
          }

          def combine(errors: List[String]): RpcResponse =
            if (aggregate.isEmpty) // had no errors; return a  VoidResponse
              VoidResponse
            else // return the cumulative error
              EnsimeServerError(aggregate mkString ", ")
        }))
      }
    case req @ RefactorReq(_, _, _) =>
      val original = sender
      context.actorOf(Props(new Actor {
        override def preStart(): Unit = {
          context.actorOf(analyzerCreator(config.projects.map(_.id))) ! req
        }
        override def receive = {
          case res: RpcResponse =>
            original ! res
            context.stop(self)
        }
      }))
    case req @ CompletionsReq(fileInfo, _, _, _, _) =>
      withExistingModuleFor(fileInfo, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ UsesOfSymbolAtPointReq(f, _) =>
      withExistingModuleFor(f, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ SymbolAtPointReq(file, point: Int) =>
      withExistingModuleFor(file, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ DocUriAtPointReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ TypeAtPointReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ SymbolDesignationsReq(f, start, end, _) =>
      withExistingModuleFor(f, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ ImplicitInfoReq(file, range: OffsetRange) =>
      withExistingModuleFor(file, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ ExpandSelectionReq(file, start: Int, stop: Int) =>
      val f = SourceFileInfo(RawFile(file.toPath), None, None)
      withExistingModuleFor(f, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ StructureViewReq(fileInfo: SourceFileInfo) =>
      withExistingModuleFor(fileInfo, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
    case req @ UnloadFileReq(file) =>
      withExistingModuleFor(file, req)((req, moduleId) =>
        getOrSpawnNew(moduleId) forward req)
  }
}

object AnalyzerManager {
  def apply(
    broadcaster: ActorRef,
    creator: List[EnsimeProjectId] => Props
  )(
    implicit
    config: EnsimeConfig
  ) = Props(new AnalyzerManager(broadcaster, creator, config))
}
