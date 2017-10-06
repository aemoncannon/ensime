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
) extends Actor
    with ActorLogging
    with Stash {

  private val sauron =
    context.actorOf(analyzerCreator(config.projects.map(_.id)))
  // maps the active modules to their analyzers
  private var analyzers: Map[EnsimeProjectId, ActorRef] = Map.empty
  private def getOrSpawnNew(optionalId: Option[EnsimeProjectId]): ActorRef =
    optionalId match {
      case Some(id) =>
        analyzers.get(id) match {
          case Some(analyzer) =>
            analyzer
          case None =>
            val name        = s"${id.project}_${id.config}"
            val newAnalyzer = context.actorOf(analyzerCreator(id :: Nil), name)
            analyzers += (id -> newAnalyzer)
            newAnalyzer
        }
      case None =>
        sauron
    }

  override def preStart(): Unit = {
    // for legacy clients on startup
    broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
    broadcaster ! Broadcaster.Persist(FullTypeCheckCompleteEvent)
  }

  override def receive: Receive = ready

  private def ready: Receive = withLabel("ready") {
    case req @ RestartScalaCompilerReq(id, _) =>
      if (analyzers.isEmpty)
        broadcaster ! AnalyzerReadyEvent
      else
        id match {
          case Some(projectId) =>
            analyzers.get(projectId).foreach(_ forward req)
          case None =>
            analyzers.values foreach (_ forward req)
        }
    case req @ UnloadAllReq =>
      analyzers.foreach {
        case (_, analyzer) => analyzer forward req
      }
    case req @ TypecheckModule(moduleId) =>
      getOrSpawnNew(Some(moduleId)) forward req
    case req @ RemoveFileReq(file: File) =>
      val fileInfo = SourceFileInfo(RawFile(file.toPath), None, None)
      getOrSpawnNew(config.findProject(fileInfo)) forward req
    case req @ TypecheckFileReq(fileInfo) =>
      getOrSpawnNew(config.findProject(fileInfo)) forward req
    case req @ TypecheckFilesReq(files) =>
      val original        = sender
      val filesPerProject = files.groupBy(config.findProject(_))

      context.actorOf(Props(new Actor {
        private var remaining               = filesPerProject.size
        private var aggregate: List[String] = List.empty

        override def preStart: Unit =
          for ((optionalModuleId, list) <- filesPerProject)
            getOrSpawnNew(optionalModuleId) ! TypecheckFilesReq(list)

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

    case req @ RefactorReq(_, _, _) =>
      val original = sender
      context.actorOf(Props(new Actor {
        override def preStart(): Unit =
          context.actorOf(analyzerCreator(config.projects.map(_.id))) ! req
        override def receive = {
          case res: RpcResponse =>
            original ! res
            context.stop(self)
        }
      }))
    case req @ CompletionsReq(fileInfo, _, _, _, _) =>
      getOrSpawnNew(config.findProject(fileInfo)) forward req
    case req @ FqnOfSymbolAtPointReq(file, point) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ FqnOfTypeAtPointReq(file, point) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ SymbolAtPointReq(file, point: Int) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ DocUriAtPointReq(file, range: OffsetRange) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ TypeAtPointReq(file, range: OffsetRange) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ SymbolDesignationsReq(file, start, end, _) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ ImplicitInfoReq(file, range: OffsetRange) =>
      getOrSpawnNew(config.findProject(file)) forward req
    case req @ ExpandSelectionReq(file, start: Int, stop: Int) =>
      val fileInfo = SourceFileInfo(RawFile(file.toPath), None, None)
      getOrSpawnNew(config.findProject(fileInfo)) forward req
    case req @ StructureViewReq(fileInfo: SourceFileInfo) =>
      getOrSpawnNew(config.findProject(fileInfo)) forward req
    case req @ UnloadFileReq(file) =>
      getOrSpawnNew(config.findProject(file)) forward req
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
