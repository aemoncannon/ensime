// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.event.LoggingReceive
import org.ensime.api._
import org.ensime.indexer.SearchService
import org.ensime.util.option._
import org.scaladebugger.api.dsl.Implicits._
import org.scaladebugger.api.lowlevel.breakpoints.BreakpointRequestInfo
import org.scaladebugger.api.lowlevel.events.EventType
import org.scaladebugger.api.lowlevel.events.misc.NoResume
import org.scaladebugger.api.lowlevel.requests.properties.SuspendPolicyProperty
import org.scaladebugger.api.profiles.traits.info._
import org.scaladebugger.api.virtualmachines.{
  ObjectCache,
  ScalaVirtualMachine
}
import SourceMap._
import StructureConverter._

import scala.util.{ Failure, Success, Try }

/**
 * Contains helper methods to initialize the DebugActor class.
 */
object DebugActor {

  /**
   * Creates a new instance of the Akka props for the DebugActor class.
   *
   * @param broadcaster The actor reference to serve as the broadcaster of
   *                    messages to the client
   * @param search
   * @param config The Ensime configuration used for source file lookup
   * @return The new Akka props instance
   */
  def props(broadcaster: ActorRef,
            search: SearchService)(implicit config: EnsimeConfig): Props =
    Props(new DebugActor(broadcaster, search, config))
}

/**
 * Represents the main entrypoint into the debugging interface of Ensime.
 *
 * @param broadcaster The actor used to send messages that should be broadcasted
 *                    to all listening clients
 * @param config The Ensime-specific configuration to associate with the
 *               debugger
 */
class DebugActor private (
  private val broadcaster: ActorRef,
  private implicit val search: SearchService,
  private implicit val config: EnsimeConfig
) extends Actor
    with ActorLogging {
  private val vmm: VirtualMachineManager = new VirtualMachineManager(
    // Signal to the user that the JVM has disconnected
    globalStopFunc = s => broadcaster ! DebugVmDisconnectEvent
  )

  // Bind our event handlers (breakpoint, step, thread start, etc.) before the
  // start of each VM
  vmm.withDummyVM(bindEventHandlers)

  /**
   * Receives user-based events and processes them.
   *
   * @return The response to the user event
   */
  override def receive: Receive = LoggingReceive {
    // ========================================================================
    case DebugAttachReq(hostname, port) =>
      vmm.stop()

      Try(vmm.start(VmAttach(hostname, port))) match {
        case Success(_) =>
          sender ! DebugVmSuccess()
        case Failure(t) =>
          log.error(t, "Failure during VM startup")
          val message = t.toString
          DebugVmError(1, message)
      }

    // ========================================================================
    case DebugActiveVmReq =>
      // Send a true response if the VM is still available, otherwise false
      sender ! withVM(_ => TrueResponse)

    // ========================================================================
    case DebugStopReq =>
      sender ! withVM(s => {
        // Force JVM exit if mode indicates to do so
        if (vmm.activeMode.exists(_.shouldExit))
          s.underlyingVirtualMachine.exit(0)

        vmm.stop()
        TrueResponse
      })

    // ========================================================================
    case DebuggerShutdownEvent =>
      vmm.stop()
      context.stop(self)

    // ========================================================================
    case DebugRunReq =>
      sender ! withVM(s => {
        s.resume()
        TrueResponse
      })

    // ========================================================================
    case DebugContinueReq(threadId) =>
      sender ! withThread(threadId.id, {
        case (s, t) =>
          s.resume()
          TrueResponse
      })

    // ========================================================================
    case DebugSetBreakReq(file, line: Int) =>
      sender ! withVM { s =>
        val fileName = toJdi(file).getOrThrow(s"could not resolve $file")
        val options  = Seq(SuspendPolicyProperty.AllThreads, NoResume)

        s.tryGetOrCreateBreakpointRequest(fileName, line, options: _*) match {
          case Success(bp) =>
            val isPending = s.isBreakpointRequestPending(fileName, line)

            if (!isPending) {
              bgMessage(s"Resolved breakpoint at: $fileName : $line")
            } else {
              bgMessage(
                s"Location not loaded ($fileName,$line). Request is pending..."
              )
            }

            TrueResponse
          case Failure(ex) =>
            FalseResponse
        }
      }

    // ========================================================================
    case DebugClearBreakReq(file, line: Int) =>
      val filename = toJdi(file).getOrThrow(s"could not resolve $file")

      vmm.withVM(_.removeBreakpointRequests(filename, line))
      vmm.withDummyVM(_.removeBreakpointRequests(filename, line))

      // Always send true response
      sender ! TrueResponse

    // ========================================================================
    case DebugClearAllBreaksReq =>
      vmm.withVM(_.removeAllBreakpointRequests())
      vmm.withDummyVM(_.removeAllBreakpointRequests())
      sender ! TrueResponse

    // ========================================================================
    case DebugListBreakpointsReq =>
      val (activeBreakpoints, pendingBreakpoints) = vmm
        .withVM(s => {
          val bps = s.breakpointRequests

          (bps.filterNot(_.isPending), bps.filter(_.isPending))
        })
        .map {
          case (a, p) =>
            def convert(b: Seq[BreakpointRequestInfo]): Seq[Breakpoint] =
              // note: silently dropping breakpoints that we fail to resolve
              b.map(b2 => (fromJdi(b2.fileName), b2.lineNumber)).collect {
                case (Some(jdiFile), line) => Breakpoint(jdiFile, line)
              }

            (convert(a).toList, convert(p).toList)
        }
        .getOrElse((Nil, Nil))

      sender ! BreakpointList(activeBreakpoints, pendingBreakpoints)

    // ========================================================================
    case DebugNextReq(threadId: DebugThreadId) =>
      sender ! withThread(threadId.id, {
        case (s, t) =>
          s.stepOverLine(t)
          TrueResponse
      })

    // ========================================================================
    case DebugStepReq(threadId: DebugThreadId) =>
      sender ! withThread(threadId.id, {
        case (s, t) =>
          s.stepIntoLine(t)
          TrueResponse
      })

    // ========================================================================
    case DebugStepOutReq(threadId: DebugThreadId) =>
      sender ! withThread(threadId.id, {
        case (s, t) =>
          s.stepOutLine(t)
          TrueResponse
      })

    // ========================================================================
    case DebugLocateNameReq(threadId: DebugThreadId, name: String) =>
      sender ! withThread(
        threadId.id, {
          case (s, t) =>
            if (name == "this") {
              t.tryTopFrame
                .flatMap(_.tryThisObject)
                .map(_.cache())
                .map {
                  case objRef => DebugObjectReference(objRef.uniqueId)
                }
                .getOrElse(FalseResponse)
            } else {
              val result = t.findVariableByName(name).map(_.cache())
              result.flatMap {
                case v: IndexedVariableInfo =>
                  Some(
                    DebugStackSlot(DebugThreadId(t.uniqueId),
                                   v.frameIndex,
                                   v.offsetIndex)
                  )
                case f: FieldVariableInfo =>
                  f.parent match {
                    case Left(o) =>
                      o.cache()
                      Some(DebugObjectField(DebugObjectId(o.uniqueId), f.name))
                    case Right(r) =>
                      log.error(
                        s"Field $name is unable to be located because it is static!"
                      )
                      None
                  }
                case x =>
                  log.error(s"Unknown value: $x")
                  None
              }.getOrElse(FalseResponse)
            }
        }
      )

    // ========================================================================
    case DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) =>
      sender ! withThread(
        threadId.id, {
          case (s, t) =>
            val frames = t.frames(index, count)

            // Cache each stack frame's "this" reference
            frames.foreach(_.thisObjectOption.foreach(_.cache()))

            val ensimeFrames = frames.map(convertStackFrame)

            DebugBacktrace(ensimeFrames.toList,
                           DebugThreadId(t.uniqueId),
                           t.name)
        }
      )

    // ========================================================================
    case DebugValueReq(location) =>
      sender ! withVM(
        s =>
          lookupValue(s, s.cache, location)
            .map(_.cache())
            .map(convertValue)
            .getOrElse(FalseResponse)
      )

    // ========================================================================
    case DebugToStringReq(threadId, location) =>
      sender ! withVM(
        s =>
          lookupValue(s, s.cache, location)
            .map(_.cache())
            .map(_.toPrettyString)
            .map(StringResponse(_))
            .getOrElse(FalseResponse)
      )

    // ========================================================================
    case DebugSetValueReq(location, newValue) =>
      sender ! withVM(s => {
        location match {
          case DebugStackSlot(threadId, frame, offset) =>
            s.tryThread(threadId.id) match {
              case Success(t) =>
                val response = suspendAndExecute(
                  t, {
                    // Find the variable and set its value
                    val variable = t.findVariableByIndex(frame, offset)
                    val result = variable match {
                      case Some(v) =>
                        // NOTE: Casting only converts to AnyVal or String, so we can
                        //       assume that a successful cast yielded one or the other,
                        //       but this might not be the case in the future
                        val actualNewValue =
                          v.typeInfo.castLocal(newValue) match {
                            case st: String => s.createRemotely(st)
                            case av         => s.createRemotely(av.asInstanceOf[AnyVal])
                          }

                        v.trySetValueFromInfo(actualNewValue)
                      case None =>
                        Failure(
                          new Throwable(
                            s"Unable to find variable at frame $frame and offset $offset"
                          )
                        )
                    }

                    result.map(_ => TrueResponse).getOrElse(FalseResponse)
                  }
                )

                response.failed.foreach(
                  log.error(
                    _,
                    s"Failed to set variable at $location to $newValue!"
                  )
                )

                response.getOrElse(FalseResponse)

              case Failure(_) =>
                log.error(s"Unknown thread $threadId for debug-set-value")
                FalseResponse
            }
          case unknown =>
            log.error(
              s"Unsupported location type for debug-set-value.: $unknown"
            )
            FalseResponse
        }
      })
  }

  // ==========================================================================

  /**
   * Sends a background message through the broadcaster.
   *
   * @param msg The message content to send as a background message
   */
  private def bgMessage(msg: String): Unit =
    broadcaster ! SendBackgroundMessageEvent(msg)

  /**
   * Attempts to invoke the provided action against the active VM. If no active
   * VM is available, falls back to invoking the action against a dummy VM that
   * will be applied to the active VM when started.
   *
   * @param action The action to execute against the virtual machine
   * @tparam T The type of RpcResponse to return from the invocation
   * @return An RPC response as the result of the action or a false response
   *         if the action fails or VM is unavailable
   */
  private def withVM[T <: RpcResponse](
    action: ScalaVirtualMachine => T
  ): RpcResponse = {
    val result = vmm.withVM(action).orElse(vmm.withDummyVM(action))

    // Report error information
    result.failed.foreach(log.warning("Failed to process VM action: {}", _))

    result.getOrElse(FalseResponse)
  }

  /**
   * Attempts to invoke the provided action against the specified thread.
   *
   * @param threadId The unique id of the thread to execute against
   * @param action   The action to execute against the thread
   * @tparam T The type of RpcResponse to return from the invocation
   * @return An RPC response as the result of the action or a false response
   *         if the action fails or thread is unavailable
   */
  private def withThread[T <: RpcResponse](
    threadId: Long,
    action: (ScalaVirtualMachine, ThreadInfo) => T
  ): RpcResponse =
    withVM(s => {
      val result = s
        .tryThread(threadId)
        .map(_.cache())
        .flatMap(t => suspendAndExecute(t, action(s, t)))

      // Report error information
      result.failed.foreach(
        log.warning(s"Unable to retrieve thread with id: $threadId - {}", _)
      )

      result.getOrElse(FalseResponse)
    })

  /**
   * Suspends the given thread, performs the action, and resumes the thread.
   *
   * @param threadInfo The thread to suspend
   * @param action The action to perform
   * @tparam T The return type of the action
   * @return Success containing the result of the action, otherwise a failure
   */
  private def suspendAndExecute[T](threadInfo: ThreadInfo,
                                   action: => T): Try[T] = {
    Try(threadInfo.suspend())
    val result = Try(action)
    Try(threadInfo.resume())
    result
  }

  /**
   * Finds a value using the provided object cache and location information.
   *
   * @param scalaVirtualMachine The virtual machine to use as a fallback in
   *                            various cases when the cache is empty
   * @param objectCache The object cache used to retrieve the value or
   *                    another object associated with the value
   * @param location The Ensime location information for the value to retrieve
   * @return Some value profile if the value is found, otherwise None
   */
  private def lookupValue(
    scalaVirtualMachine: ScalaVirtualMachine,
    objectCache: ObjectCache,
    location: DebugLocation
  ): Option[ValueInfo] = location match {
    // Retrieves cached object
    case DebugObjectReference(objectId) =>
      log.debug(s"Looking up object from reference $objectId")
      objectCache.load(objectId.id)

    // Uses cached object with id to find associated field
    // Caches retrieved field object
    case DebugObjectField(objectId, fieldName) =>
      log.debug(
        s"Looking up object field from reference $objectId and field $fieldName"
      )
      objectCache
        .load(objectId.id)
        .map(_.field(fieldName))
        .map(_.toValueInfo.cache())

    // Uses cached object with id as array to find element
    // Caches retrieved element object
    case DebugArrayElement(objectId, index) =>
      log.debug(
        s"Looking up array element from reference $objectId and index $index"
      )
      objectCache
        .load(objectId.id)
        .flatMap {
          case a if a.isArray => Some(a.toArrayInfo)
          case _              => None
        }
        .map(_.value(index).cache())

    // Caches retrieved slot object
    case DebugStackSlot(threadId, frame, offset) =>
      log.debug(
        s"Looking up object from thread $threadId, frame $frame, and offset $offset"
      )
      val s = scalaVirtualMachine
      objectCache
        .load(threadId.id)
        .orElse(s.tryThread(threadId.id).toOption)
        .flatMap {
          case t if t.isThread => Some(t.toThreadInfo)
          case _               => None
        }
        .flatMap(_.findVariableByIndex(frame, offset))
        .map(_.toValueInfo.cache())

    // Unrecognized location request, so return nothing
    case _ =>
      log.error(s"Unknown location: $location")
      None
  }

  /**
   * Creates a variety of event listeners that broadcast messages and log
   * information when those associated events occur.
   *
   * @param scalaVirtualMachine The JVM whose events to listen to
   */
  private def bindEventHandlers(
    scalaVirtualMachine: ScalaVirtualMachine
  ): Unit = {
    // Send start event to client when received
    scalaVirtualMachine
      .onUnsafeVMStart()
      .foreach(_ => broadcaster ! DebugVmStartEvent)

    // Breakpoint Event - capture and broadcast breakpoint information
    scalaVirtualMachine
      .createEventListener(EventType.BreakpointEventType)
      .foreach { e =>
        val be              = e.toBreakpointEvent
        val l: LocationInfo = be.location

        fromJdi(l.sourcePath) match {
          case Some(resolved) =>
            val t: ThreadInfo = be.thread
            broadcaster ! DebugBreakEvent(
              DebugThreadId(t.uniqueId),
              t.name,
              resolved,
              l.lineNumber
            )
          case None =>
            val sn = l.sourceName
            val ln = l.lineNumber
            log.warning(s"Breakpoint position not found: $sn : $ln")
        }
      }

    // Step Event - capture and broadcast step information
    scalaVirtualMachine
      .createEventListener(EventType.StepEventType)
      .foreach(e => {
        val se              = e.toStepEvent
        val l: LocationInfo = se.location

        fromJdi(l.sourcePath) match {
          case Some(resolved) =>
            val t: ThreadInfo = se.thread
            broadcaster ! DebugStepEvent(
              DebugThreadId(t.uniqueId),
              t.name,
              resolved,
              l.lineNumber
            )
          case None =>
            val sn = l.sourceName
            val ln = l.lineNumber
            log.warning(s"Step position not found: $sn : $ln")
        }
      })

    scalaVirtualMachine
      .createEventListener(EventType.ExceptionEventType)
      .foreach { e =>
        val ee                       = e.toExceptionEvent
        val t                        = ee.thread
        val ex                       = ee.exception
        val ce: Option[LocationInfo] = Option(ee.catchLocation).flatten // can be null
        val resolved                 = ce.flatMap(loc => fromJdi(loc.sourcePath))

        broadcaster ! DebugExceptionEvent(
          ex.uniqueId,
          DebugThreadId(t.uniqueId),
          t.name,
          resolved,
          ce.map(_.lineNumber)
        )
      }

    // Exception Event - capture and broadcast exception information
    // Listen for all uncaught exceptions, suspending the entire JVM when we
    // encounter an uncaught exception event, and cache the exception
    scalaVirtualMachine
      .getOrCreateAllExceptionsRequest(
        notifyCaught = false,
        notifyUncaught = true,
        SuspendPolicyProperty.AllThreads
      )
      .foreach { e =>
        // Cache the exception object
        e.exception.cache()

        val t        = e.thread
        val ex       = e.exception
        val location = Option(e.catchLocation).flatten
        val resolved = location.flatMap(l => fromJdi(l.sourcePath))

        broadcaster ! DebugExceptionEvent(
          ex.uniqueId,
          DebugThreadId(t.uniqueId),
          t.name,
          resolved,
          location.map(_.lineNumber)
        )
      }

    // Thread Start Event - capture and broadcast associated thread
    scalaVirtualMachine
      .onUnsafeThreadStart(SuspendPolicyProperty.NoThread)
      .foreach(t => {
        broadcaster ! DebugThreadStartEvent(DebugThreadId(t.thread.uniqueId))
      })

    // Thread Death Event - capture and broadcast associated thread
    scalaVirtualMachine
      .onUnsafeThreadDeath(SuspendPolicyProperty.NoThread)
      .foreach(t => {
        broadcaster ! DebugThreadDeathEvent(DebugThreadId(t.thread.uniqueId))
      })
  }
}
