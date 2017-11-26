// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File

import spray.json._

import scalaz.deriving

/**
 * There should be exactly one `RpcResponseEnvelope` in response to an
 * `RpcRequestEnvelope`. If the `callId` is empty, the response is
 * an asynchronous event.
 */
@deriving(JsReader, JsWriter)
final case class RpcResponseEnvelope(
  callId: Option[Int],
  payload: EnsimeServerMessage
)

@deriving(JsReader, JsWriter)
sealed trait EnsimeServerMessage

/**
 * A message that the server can send to the client at any time.
 */
@deriving(JsReader, JsWriter)
sealed trait EnsimeEvent extends EnsimeServerMessage

//////////////////////////////////////////////////////////////////////
// Contents of the payload

@deriving(JsReader, JsWriter)
sealed trait RpcResponse extends EnsimeServerMessage
@deriving(JsReader, JsWriter)
final case class EnsimeServerError(description: String) extends RpcResponse

@deriving(JsReader, JsWriter)
case object DebuggerShutdownEvent

@deriving(JsReader, JsWriter)
sealed trait DebugVmStatus extends RpcResponse

// must have redundant status: String to match legacy API
@deriving(JsReader, JsWriter)
final case class DebugVmSuccess(
  status: String = "success"
) extends DebugVmStatus
@deriving(JsReader, JsWriter)
final case class DebugVmError(
  errorCode: Int,
  details: String,
  status: String = "error"
) extends DebugVmStatus

@deriving(JsReader, JsWriter)
sealed trait GeneralSwankEvent extends EnsimeEvent
@deriving(JsReader, JsWriter)
sealed trait DebugEvent extends EnsimeEvent

@deriving(JsReader, JsWriter)
final case class GreetingInfo(
  pid: Option[Int] = None,
  implementation: EnsimeImplementation = EnsimeImplementation("ENSIME"),
  version: String = "3.0.0"
) extends EnsimeEvent

/**
 * Generic background notification.
 *
 * NOTE: codes will be deprecated, preferring @deriving(JsReader, JsWriter)
 sealed families.
 */
@deriving(JsReader, JsWriter)
final case class SendBackgroundMessageEvent(
  detail: String,
  code: Int = 105
) extends GeneralSwankEvent

/** Initial indexing has completed */
@deriving(JsReader, JsWriter)
case object IndexerReadyEvent extends GeneralSwankEvent

/** The presentation compiler was restarted. Existing `:type-id`s are invalid. */
@deriving(JsReader, JsWriter)
case object CompilerRestartedEvent extends GeneralSwankEvent

/** The presentation compiler has invalidated all existing notes.  */
@deriving(JsReader, JsWriter)
case object ClearAllScalaNotesEvent extends GeneralSwankEvent

/** The presentation compiler has invalidated all existing notes.  */
@deriving(JsReader, JsWriter)
case object ClearAllJavaNotesEvent extends GeneralSwankEvent

@deriving(JsReader, JsWriter)
final case class Note(
  file: String,
  msg: String,
  severity: NoteSeverity,
  beg: Int,
  end: Int,
  line: Int,
  col: Int
) extends RpcResponse

/** The presentation compiler is providing notes: e.g. errors, warnings. */
@deriving(JsReader, JsWriter)
final case class NewScalaNotesEvent(
  isFull: Boolean,
  notes: List[Note]
) extends GeneralSwankEvent

/** The presentation compiler is providing notes: e.g. errors, warnings. */
@deriving(JsReader, JsWriter)
final case class NewJavaNotesEvent(
  isFull: Boolean,
  notes: List[Note]
) extends GeneralSwankEvent

/** The debugged VM has stepped to a new location and is now paused awaiting control. */
@deriving(JsReader, JsWriter)
final case class DebugStepEvent(
  threadId: DebugThreadId,
  threadName: String,
  file: EnsimeFile,
  line: Int
) extends DebugEvent

/** The debugged VM has stopped at a breakpoint. */
@deriving(JsReader, JsWriter)
final case class DebugBreakEvent(
  threadId: DebugThreadId,
  threadName: String,
  file: EnsimeFile,
  line: Int
) extends DebugEvent

/** The debugged VM has started. */
@deriving(JsReader, JsWriter)
case object DebugVmStartEvent extends DebugEvent

/** The debugger has disconnected from the debugged VM. */
@deriving(JsReader, JsWriter)
case object DebugVmDisconnectEvent extends DebugEvent

/** The debugged VM has thrown an exception and is now paused waiting for control. */
@deriving(JsReader, JsWriter)
final case class DebugExceptionEvent(
  exception: Long,
  threadId: DebugThreadId,
  threadName: String,
  file: Option[EnsimeFile],
  line: Option[Int]
) extends DebugEvent

/** A new thread has started. */
@deriving(JsReader, JsWriter)
final case class DebugThreadStartEvent(threadId: DebugThreadId)
    extends DebugEvent

/** A thread has died. */
@deriving(JsReader, JsWriter)
final case class DebugThreadDeathEvent(threadId: DebugThreadId)
    extends DebugEvent

/** Communicates stdout/stderr of debugged VM to client. */
@deriving(JsReader, JsWriter)
final case class DebugOutputEvent(body: String) extends DebugEvent

@deriving(JsReader, JsWriter)
case object VoidResponse extends RpcResponse

@deriving(JsReader, JsWriter)
final case class RefactorFailure(
  procedureId: Int,
  reason: String,
  status: scala.Symbol = 'failure // redundant field
) extends RpcResponse

trait RefactorProcedure {
  def procedureId: Int
  def refactorType: RefactorType
}

@deriving(JsReader, JsWriter)
final case class RefactorDiffEffect(
  procedureId: Int,
  refactorType: RefactorType,
  diff: File
) extends RpcResponse
    with RefactorProcedure

@deriving(JsReader, JsWriter)
sealed abstract class RefactorDesc(val refactorType: RefactorType)

@deriving(JsReader, JsWriter)
final case class InlineLocalRefactorDesc(file: File, start: Int, end: Int)
    extends RefactorDesc(RefactorType.InlineLocal)

@deriving(JsReader, JsWriter)
final case class RenameRefactorDesc(newName: String,
                                    file: File,
                                    start: Int,
                                    end: Int)
    extends RefactorDesc(RefactorType.Rename)

@deriving(JsReader, JsWriter)
final case class ExtractMethodRefactorDesc(methodName: String,
                                           file: File,
                                           start: Int,
                                           end: Int)
    extends RefactorDesc(RefactorType.ExtractMethod)

@deriving(JsReader, JsWriter)
final case class ExtractLocalRefactorDesc(name: String,
                                          file: File,
                                          start: Int,
                                          end: Int)
    extends RefactorDesc(RefactorType.ExtractLocal)

@deriving(JsReader, JsWriter)
final case class OrganiseImportsRefactorDesc(file: File)
    extends RefactorDesc(RefactorType.OrganizeImports)

@deriving(JsReader, JsWriter)
final case class AddImportRefactorDesc(qualifiedName: String, file: File)
    extends RefactorDesc(RefactorType.AddImport)

@deriving(JsReader, JsWriter)
final case class ExpandMatchCasesDesc(file: File, start: Int, end: Int)
    extends RefactorDesc(RefactorType.ExpandMatchCases)

@deriving(JsReader, JsWriter)
sealed trait PatchOp {
  def start: Int
}

@deriving(JsReader, JsWriter)
final case class PatchInsert(
  start: Int,
  text: String
) extends PatchOp

@deriving(JsReader, JsWriter)
final case class PatchDelete(
  start: Int,
  end: Int
) extends PatchOp

@deriving(JsReader, JsWriter)
final case class PatchReplace(
  start: Int,
  end: Int,
  text: String
) extends PatchOp

@deriving(JsReader, JsWriter)
sealed trait EntityInfo extends RpcResponse {
  def name: String
  def members: Iterable[EntityInfo]
}

object SourceSymbol {
  val allSymbols: List[SourceSymbol] = List(
    ObjectSymbol,
    ClassSymbol,
    TraitSymbol,
    PackageSymbol,
    ConstructorSymbol,
    ImportedNameSymbol,
    TypeParamSymbol,
    ParamSymbol,
    VarFieldSymbol,
    ValFieldSymbol,
    OperatorFieldSymbol,
    VarSymbol,
    ValSymbol,
    FunctionCallSymbol,
    ImplicitConversionSymbol,
    ImplicitParamsSymbol,
    DeprecatedSymbol
  )
}

@deriving(JsReader, JsWriter)
sealed trait SourceSymbol

@deriving(JsReader, JsWriter)
case object ObjectSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ClassSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object TraitSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object PackageSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ConstructorSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ImportedNameSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object TypeParamSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ParamSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object VarFieldSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ValFieldSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object OperatorFieldSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object VarSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ValSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object FunctionCallSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ImplicitConversionSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object ImplicitParamsSymbol extends SourceSymbol
@deriving(JsReader, JsWriter)
case object DeprecatedSymbol extends SourceSymbol

@deriving(JsReader, JsWriter)
sealed trait PosNeeded
@deriving(JsReader, JsWriter)
case object PosNeededNo extends PosNeeded
@deriving(JsReader, JsWriter)
case object PosNeededAvail extends PosNeeded
@deriving(JsReader, JsWriter)
case object PosNeededYes extends PosNeeded

@deriving(JsReader, JsWriter)
sealed trait SourcePosition extends RpcResponse
@deriving(JsReader, JsWriter)
final case class EmptySourcePosition() extends SourcePosition
@deriving(JsReader, JsWriter)
final case class OffsetSourcePosition(file: EnsimeFile, offset: Int)
    extends SourcePosition
@deriving(JsReader, JsWriter)
final case class LineSourcePosition(file: EnsimeFile, line: Int)
    extends SourcePosition

@deriving(JsReader, JsWriter)
final case class PositionHint(position: SourcePosition, preview: Option[String])
@deriving(JsReader, JsWriter)
final case class SourcePositions(positions: List[PositionHint])
    extends RpcResponse

// See if `TypeInfo` can be used instead
@deriving(JsReader, JsWriter)
final case class ClassInfo(scalaName: Option[String],
                           fqn: String,
                           declAs: DeclaredAs,
                           sourcePosition: Option[SourcePosition])

@deriving(JsReader, JsWriter)
final case class HierarchyInfo(ancestors: List[ClassInfo],
                               inheritors: List[ClassInfo])
    extends RpcResponse

@deriving(JsReader, JsWriter)
final case class PackageInfo(
  name: String,
  fullName: String,
  // n.b. members should be sorted by name for consistency
  members: Seq[EntityInfo]
) extends EntityInfo {
  require(members == members.sortBy(_.name), "members should be sorted by name")
}

@deriving(JsReader, JsWriter)
sealed trait SymbolSearchResult extends RpcResponse {
  def name: String
  def localName: String
  def declAs: DeclaredAs
  def pos: Option[SourcePosition]
}

@deriving(JsReader, JsWriter)
final case class TypeSearchResult(
  name: String,
  localName: String,
  declAs: DeclaredAs,
  pos: Option[SourcePosition]
) extends SymbolSearchResult

@deriving(JsReader, JsWriter)
final case class MethodSearchResult(
  name: String,
  localName: String,
  declAs: DeclaredAs,
  pos: Option[SourcePosition],
  ownerName: String
) extends SymbolSearchResult

// what is the point of these types?
@deriving(JsReader, JsWriter)
final case class ImportSuggestions(symLists: List[List[SymbolSearchResult]])
    extends RpcResponse
@deriving(JsReader, JsWriter)
final case class SymbolSearchResults(syms: List[SymbolSearchResult])
    extends RpcResponse

@deriving(JsReader, JsWriter)
final case class SymbolDesignations(
  file: EnsimeFile,
  syms: List[SymbolDesignation]
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class SymbolDesignation(
  start: Int,
  end: Int,
  symType: SourceSymbol
)

@deriving(JsReader, JsWriter)
final case class SymbolInfo(
  name: String,
  localName: String,
  declPos: Option[SourcePosition],
  `type`: TypeInfo
) extends RpcResponse {
  def tpe = `type`
}

@deriving(JsReader, JsWriter)
final case class Op(
  op: String,
  description: String
)

@deriving(JsReader, JsWriter)
final case class MethodBytecode(
  className: String,
  methodName: String,
  methodSignature: Option[String],
  byteCode: List[Op],
  startLine: Int,
  endLine: Int
)

@deriving(JsReader, JsWriter)
final case class CompletionInfo(
  typeInfo: Option[TypeInfo],
  name: String,
  relevance: Int,
  toInsert: Option[String],
  isInfix: Boolean = false
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class CompletionInfoList(
  prefix: String,
  completions: List[CompletionInfo]
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class Breakpoint(file: EnsimeFile, line: Int) extends RpcResponse
@deriving(JsReader, JsWriter)
final case class BreakpointList(active: List[Breakpoint],
                                pending: List[Breakpoint])
    extends RpcResponse

/**
 * A debugger thread id.
 */
@deriving(JsReader, JsWriter)
final case class DebugThreadId(id: Long) extends AnyVal

object DebugThreadId {

  /**
   * Create a ThreadId from a String representation
   * @param s A Long encoded as a string
   * @return A ThreadId
   */
  @deprecating("no code in the API")
  def apply(s: String): DebugThreadId =
    new DebugThreadId(s.toLong)
}

@deriving(JsReader, JsWriter)
final case class DebugObjectId(id: Long)

object DebugObjectId {

  /**
   * Create a DebugObjectId from a String representation
   * @param s A Long encoded as a string
   * @return A DebugObjectId
   */
  @deprecating("no code in the API")
  def apply(s: String): DebugObjectId =
    new DebugObjectId(s.toLong)
}

// these are used in the queries as well, shouldn't be raw response
@deriving(JsReader, JsWriter)
sealed trait DebugLocation extends RpcResponse

@deriving(JsReader, JsWriter)
final case class DebugObjectReference(objectId: DebugObjectId)
    extends DebugLocation

object DebugObjectReference {
  def apply(objId: Long): DebugObjectReference =
    new DebugObjectReference(DebugObjectId(objId))
}

@deriving(JsReader, JsWriter)
final case class DebugStackSlot(threadId: DebugThreadId,
                                frame: Int,
                                offset: Int)
    extends DebugLocation

@deriving(JsReader, JsWriter)
final case class DebugArrayElement(objectId: DebugObjectId, index: Int)
    extends DebugLocation

@deriving(JsReader, JsWriter)
final case class DebugObjectField(objectId: DebugObjectId, field: String)
    extends DebugLocation

@deriving(JsReader, JsWriter)
sealed trait DebugValue extends RpcResponse {
  def typeName: String
}

@deriving(JsReader, JsWriter)
final case class DebugNullValue(
  typeName: String
) extends DebugValue

@deriving(JsReader, JsWriter)
final case class DebugPrimitiveValue(
  summary: String,
  typeName: String
) extends DebugValue

@deriving(JsReader, JsWriter)
final case class DebugObjectInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: DebugObjectId
) extends DebugValue

@deriving(JsReader, JsWriter)
final case class DebugStringInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: DebugObjectId
) extends DebugValue

@deriving(JsReader, JsWriter)
final case class DebugArrayInstance(
  length: Int,
  typeName: String,
  elementTypeName: String,
  objectId: DebugObjectId
) extends DebugValue

@deriving(JsReader, JsWriter)
final case class DebugClassField(
  index: Int,
  name: String,
  typeName: String,
  summary: String
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class DebugStackLocal(
  index: Int,
  name: String,
  summary: String,
  typeName: String
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class DebugStackFrame(
  index: Int,
  locals: List[DebugStackLocal],
  numArgs: Int,
  className: String,
  methodName: String,
  pcLocation: LineSourcePosition,
  thisObjectId: DebugObjectId
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class DebugBacktrace(
  frames: List[DebugStackFrame],
  threadId: DebugThreadId,
  threadName: String
) extends RpcResponse

@deriving(JsReader, JsWriter)
final case class NamedTypeMemberInfo(
  name: String,
  `type`: TypeInfo,
  pos: Option[SourcePosition],
  signatureString: Option[String], // the FQN descriptor
  declAs: DeclaredAs
) extends EntityInfo {
  override def members = List.empty
  def tpe              = `type`
}

@deriving(JsReader, JsWriter)
sealed trait TypeInfo extends EntityInfo {
  def name: String
  def declAs: DeclaredAs
  def fullName: String
  def typeArgs: Iterable[TypeInfo]
  def members: Iterable[EntityInfo]
  def pos: Option[SourcePosition]
  def typeParams: List[TypeInfo]

  final def declaredAs = declAs
  final def args       = typeArgs
}

@deriving(JsReader, JsWriter)
final case class BasicTypeInfo(
  name: String,
  declAs: DeclaredAs,
  fullName: String,
  typeArgs: Iterable[TypeInfo],
  members: Iterable[EntityInfo],
  pos: Option[SourcePosition],
  typeParams: List[TypeInfo]
) extends TypeInfo

@deriving(JsReader, JsWriter)
final case class ArrowTypeInfo(
  name: String,
  fullName: String,
  resultType: TypeInfo,
  paramSections: Iterable[ParamSectionInfo],
  typeParams: List[TypeInfo]
) extends TypeInfo {
  def declAs   = DeclaredAs.Nil
  def typeArgs = List.empty
  def members  = List.empty
  def pos      = None
}

@deriving(JsReader, JsWriter)
final case class ParamSectionInfo(
  params: Iterable[(String, TypeInfo)],
  isImplicit: Boolean
)

@deriving(JsReader, JsWriter)
final case class InterfaceInfo(
  `type`: TypeInfo,
  viaView: Option[String]
) extends RpcResponse {
  def tpe = `type`
}

@deriving(JsReader, JsWriter)
final case class FileRange(file: String, start: Int, end: Int)
    extends RpcResponse

@deriving(JsReader, JsWriter)
final case class EnsimeImplementation(
  name: String
)
@deriving(JsReader, JsWriter)
final case class ConnectionInfo(
  pid: Option[Int] = None,
  implementation: EnsimeImplementation = EnsimeImplementation("ENSIME"),
  version: String = "1.9.6"
) extends RpcResponse

@deriving(JsReader, JsWriter)
sealed trait ImplicitInfo

@deriving(JsReader, JsWriter)
final case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends ImplicitInfo

@deriving(JsReader, JsWriter)
final case class ImplicitParamInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends ImplicitInfo

@deriving(JsReader, JsWriter)
final case class ImplicitInfos(infos: List[ImplicitInfo]) extends RpcResponse

@deriving(JsReader, JsWriter)
sealed trait LegacyRawResponse extends RpcResponse
@deriving(JsReader, JsWriter)
case object FalseResponse extends LegacyRawResponse
@deriving(JsReader, JsWriter)
case object TrueResponse extends LegacyRawResponse
@deriving(JsReader, JsWriter)
final case class StringResponse(text: String) extends LegacyRawResponse

@deriving(JsReader, JsWriter)
final case class StructureView(view: List[StructureViewMember])
    extends RpcResponse

@deriving(JsReader, JsWriter)
final case class StructureViewMember(
  keyword: String,
  name: String,
  position: SourcePosition,
  members: List[StructureViewMember]
)
