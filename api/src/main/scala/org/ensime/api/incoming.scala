// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.api

import java.io.File

import scalaz.deriving
import scalaz.std.either._
import scalaz.std.list._
import scalaz.std.option._

import spray.json.{ JsReader, JsWriter }

import org.ensime.io.Canon
import org.ensime.sexp.{ SexpReader, SexpWriter }

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RpcRequestEnvelope(req: RpcRequest, callId: Int)

/**
 * All messages into the ENSIME server from the client are part of
 * this family.
 *
 * NOTE: we intend to simplify these messages
 * https://github.com/ensime/ensime-server/issues/845
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait RpcRequest

// queries related to connection startup
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait RpcStartupRequest extends RpcRequest

/**
 * Responds with a `ConnectionInfo`.
 */
@deprecating("Please switch to asynchronous connection handling.")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ConnectionInfoReq extends RpcStartupRequest

// related to managing the state of the analyser
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait RpcAnalyserRequest extends RpcRequest

/**
 * Request details about implicit conversions applied inside the given
 * range.
 *
 * Responds with `ImplicitInfos`.
 *
 * @param file source.
 * @param range in the file to inspect.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ImplicitInfoReq(
  file: Either[File, SourceFileInfo],
  range: OffsetRange
) extends RpcAnalyserRequest

/**
 * Tell the Analyzer that this file has been deleted. This is
 * different to simply unloading the file (which can keeps symbols
 * around).
 *
 * Responds with a `VoidResponse`.
 */
@deprecating("prefer UnloadFilesReq")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RemoveFileReq(file: File) extends RpcAnalyserRequest

/**
 * Responds with a `VoidResponse`.
 */
@deprecating("redundant query, use TypecheckFilesReq")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class TypecheckFileReq(fileInfo: SourceFileInfo)
    extends RpcAnalyserRequest

/**
 * Responds with a `VoidResponse`
 */
@deprecating("prefer UnloadFilesReq")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class UnloadFileReq(fileInfo: SourceFileInfo)
    extends RpcAnalyserRequest

/**
 * Unload the given files from the compiler. The additional `remove`
 * flag signals if previously loaded symbols should be removed (use
 * this if the user has deleted / renamed the file on disk).
 *
 * Responds with a `VoidResponse`
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class UnloadFilesReq(
  source: List[SourceFileInfo],
  remove: Boolean
) extends RpcAnalyserRequest

/**
 * Response with a `VoidResponse`.
 */
@deprecating("replaced by RestartAnalyzerReq")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class TypecheckModule(moduleId: EnsimeProjectId)
    extends RpcAnalyserRequest

/**
 * Responds with a `VoidResponse`.
 */
@deprecating("replaced by RestartAnalyzerReq")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object UnloadAllReq extends RpcAnalyserRequest

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait ReloadStrategy
object ReloadStrategy {

  /** a clean slate, client should reload all open files */
  @deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
  case object UnloadAll extends ReloadStrategy

  /**
   * compiles all project sources, e.g. project is not batch compiled.
   * Client should reload all third party files.
   */
  @deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
  case object LoadProject extends ReloadStrategy

  /** reload all the files that were previously loaded */
  @deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
  case object KeepLoaded extends ReloadStrategy
}

/**
 * Restart the scala presentation compiler for the given id, using the
 * provided file loading strategy.
 *
 * No RPC response, there will be CompilerRestartedEvent
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RestartScalaCompilerReq(
  id: Option[EnsimeProjectId],
  strategy: ReloadStrategy
) extends RpcAnalyserRequest

/**
 * Responds with a `VoidResponse`.
 */
@deprecating("should only support SourceFileInfo")
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class TypecheckFilesReq(files: List[Either[File, SourceFileInfo]])
    extends RpcAnalyserRequest

// related to searching the indexer
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait RpcSearchRequest extends RpcRequest

/**
 * Responds with `SymbolSearchResults`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class PublicSymbolSearchReq(
  keywords: List[String],
  maxResults: Int
) extends RpcSearchRequest

/**
 * Responds with [ImportSuggestions].
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ImportSuggestionsReq(
  file: Either[File, SourceFileInfo],
  point: Int,
  names: List[String],
  maxResults: Int
) extends RpcSearchRequest

/**
 * Responds with `FullyQualifiedName`
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class FqnOfSymbolAtPointReq(file: SourceFileInfo, point: Int)
    extends RpcAnalyserRequest

/**
 * Responds with `FullyQualifiedName`
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class FqnOfTypeAtPointReq(file: SourceFileInfo, point: Int)
    extends RpcAnalyserRequest

/**
 * Responds with `SourcePositions`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class UsesOfSymbolAtPointReq(
  file: SourceFileInfo,
  point: Int
) extends RpcRequest

/**
 * Responds with `HierarchyInfo`
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class HierarchyOfTypeAtPointReq(
  file: SourceFileInfo,
  point: Int
) extends RpcRequest

/**
 * Responds with `SourcePositions`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class FindUsages(fqn: String) extends RpcSearchRequest

/**
 * Responds with `HierarchyInfo`
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class FindHierarchy(fqn: String) extends RpcSearchRequest

/**
 * Responds with a `StringResponse` for the URL of the documentation if valid,
 * or `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DocUriAtPointReq(
  file: Either[File, SourceFileInfo],
  point: OffsetRange
) extends RpcAnalyserRequest

/**
 * Responds with a `CompletionInfoList`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class CompletionsReq(
  fileInfo: SourceFileInfo,
  point: Int,
  maxResults: Int,
  caseSens: Boolean,
  reload: Boolean
) extends RpcAnalyserRequest

/**
 * Responds with `TypeInfo` if valid, or `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class TypeAtPointReq(
  file: Either[File, SourceFileInfo],
  range: OffsetRange
) extends RpcAnalyserRequest

/**
 * Responds with a `SymbolInfo` if valid, or `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SymbolAtPointReq(file: Either[File, SourceFileInfo],
                                  point: Int)
    extends RpcAnalyserRequest

/**
 * Responds with a `RefactorFailure` or a `RefactorDiffEffect`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RefactorReq(
  procId: Int,
  params: RefactorDesc,
  interactive: Boolean
) extends RpcAnalyserRequest

/**
 * Request the semantic classes of symbols in the given range.
 * Intended for semantic highlighting.
 *
 * Responds with a `SymbolDesignations`.
 *
 * @param file source.
 * @param start of character offset of the input range.
 * @param end of character offset of the input range.
 * @param requestedTypes semantic classes in which we are interested.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SymbolDesignationsReq(
  file: Either[File, SourceFileInfo],
  start: Int,
  end: Int,
  requestedTypes: List[SourceSymbol]
) extends RpcAnalyserRequest

/**
 * Responds with a `FileRange`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ExpandSelectionReq(file: File, start: Int, end: Int)
    extends RpcAnalyserRequest

/**
 * Responds with a `StructureView`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class StructureViewReq(fileInfo: SourceFileInfo)
    extends RpcAnalyserRequest

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait RpcDebuggerRequest extends RpcRequest

/**
 * Query whether we are in an active debug session
 * Responds with a `TrueResponse` or a `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case object DebugActiveVmReq extends RpcDebuggerRequest

/**
 * Responds with `DebugVmStatus`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugAttachReq(hostname: String, port: String)
    extends RpcDebuggerRequest

/**
 * Responds with a `FalseResponse` or a `TrueResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object DebugStopReq extends RpcDebuggerRequest

/**
 * Responds with a `VoidResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugSetBreakReq(file: EnsimeFile, line: Int)
    extends RpcDebuggerRequest

/**
 * Responds with a `VoidResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugClearBreakReq(file: EnsimeFile, line: Int)
    extends RpcDebuggerRequest

/**
 * Responds with a `VoidResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object DebugClearAllBreaksReq extends RpcDebuggerRequest

/**
 * Responds with a `BreakpointList`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object DebugListBreakpointsReq extends RpcDebuggerRequest

/**
 * Responds with a `FalseResponse` or a `TrueResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object DebugRunReq extends RpcDebuggerRequest

/**
 * Request to continue the execution of the given thread.
 * Responds with a `FalseResponse` or a `TrueResponse`.
 * @param threadId the target debugged VM thread
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugContinueReq(threadId: DebugThreadId)
    extends RpcDebuggerRequest

/**
 * Responds with a `FalseResponse` or a `TrueResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugStepReq(threadId: DebugThreadId)
    extends RpcDebuggerRequest

/**
 * Responds with a `FalseResponse` or a `TrueResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugNextReq(threadId: DebugThreadId)
    extends RpcDebuggerRequest

/**
 * Responds with a `FalseResponse` or a `TrueResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugStepOutReq(threadId: DebugThreadId)
    extends RpcDebuggerRequest

/**
 * Responds with a `DebugLocation` if successful, or `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugLocateNameReq(threadId: DebugThreadId, name: String)
    extends RpcDebuggerRequest

/**
 * Responds with a `DebugValue` if successful, or `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugValueReq(loc: DebugLocation) extends RpcDebuggerRequest

/**
 * Responds with a `StringResponse` if successful, or `FalseResponse`.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugToStringReq(threadId: DebugThreadId, loc: DebugLocation)
    extends RpcDebuggerRequest

/**
 * Request to update a field value within the debugged VM.
 * Responds with a `TrueResponse` on success or a `FalseResponse` on failure.
 * @param loc The variable to update.
 * @param newValue The value to set, encoded as a String
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugSetValueReq(loc: DebugLocation, newValue: String)
    extends RpcDebuggerRequest

/**
 * Responds with a `DebugBacktrace`.
 * @param threadId The target debugging thread
 * @param index The index of the first frame where 0 is the lowest frame
 * @param count The number of frames to return
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class DebugBacktraceReq(threadId: DebugThreadId,
                                   index: Int,
                                   count: Int)
    extends RpcDebuggerRequest
