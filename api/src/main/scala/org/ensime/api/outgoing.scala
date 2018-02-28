// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.api

import java.io.File

import scalaz.deriving
import scalaz.std.option._
import scalaz.std.list._
import scalaz.std.tuple._

import spray.json.{ JsReader, JsWriter }

import org.ensime.sexp.{ SexpReader, SexpWriter }
import org.ensime.io.Canon

/**
 * There should be exactly one `RpcResponseEnvelope` in response to an
 * `RpcRequestEnvelope`. If the `callId` is empty, the response is
 * an asynchronous event.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RpcResponseEnvelope(
  callId: Option[Int],
  payload: EnsimeServerMessage
)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait EnsimeServerMessage

/**
 * A message that the server can send to the client at any time.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon) // , Canon)
sealed trait EnsimeEvent extends EnsimeServerMessage

//////////////////////////////////////////////////////////////////////
// Contents of the payload

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait RpcResponse extends EnsimeServerMessage
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class EnsimeServerError(description: String) extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait GeneralSwankEvent extends EnsimeEvent

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class GreetingInfo(
  pid: Option[Int] = None,
  implementation: EnsimeImplementation = EnsimeImplementation("ENSIME"),
  version: String = "3.0.0"
) extends EnsimeEvent

/**
 * Generic background notification.
 *
 * NOTE: codes will be deprecated, preferring @deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
 sealed families.
 */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SendBackgroundMessageEvent(
  detail: String,
  code: Int = 105
) extends GeneralSwankEvent

/** Initial indexing has completed */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object IndexerReadyEvent extends GeneralSwankEvent

/** The presentation compiler was restarted. Existing `:type-id`s are invalid. */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object CompilerRestartedEvent extends GeneralSwankEvent

/** The presentation compiler has invalidated all existing notes.  */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ClearAllScalaNotesEvent extends GeneralSwankEvent

/** The presentation compiler has invalidated all existing notes.  */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ClearAllJavaNotesEvent extends GeneralSwankEvent

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
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
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class NewScalaNotesEvent(
  isFull: Boolean,
  notes: List[Note]
) extends GeneralSwankEvent

/** The presentation compiler is providing notes: e.g. errors, warnings. */
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class NewJavaNotesEvent(
  isFull: Boolean,
  notes: List[Note]
) extends GeneralSwankEvent

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object VoidResponse extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RefactorFailure(
  procedureId: Int,
  reason: String,
  status: scala.Symbol = 'failure // redundant field
) extends RpcResponse

trait RefactorProcedure {
  def procedureId: Int
  def refactorType: RefactorType
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RefactorDiffEffect(
  procedureId: Int,
  refactorType: RefactorType,
  diff: File
) extends RpcResponse
    with RefactorProcedure

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed abstract class RefactorDesc(val refactorType: RefactorType)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class InlineLocalRefactorDesc(file: File, start: Int, end: Int)
    extends RefactorDesc(RefactorType.InlineLocal)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class RenameRefactorDesc(newName: String,
                                    file: File,
                                    start: Int,
                                    end: Int)
    extends RefactorDesc(RefactorType.Rename)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ExtractMethodRefactorDesc(methodName: String,
                                           file: File,
                                           start: Int,
                                           end: Int)
    extends RefactorDesc(RefactorType.ExtractMethod)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ExtractLocalRefactorDesc(name: String,
                                          file: File,
                                          start: Int,
                                          end: Int)
    extends RefactorDesc(RefactorType.ExtractLocal)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class OrganiseImportsRefactorDesc(file: File)
    extends RefactorDesc(RefactorType.OrganizeImports)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class AddImportRefactorDesc(qualifiedName: String, file: File)
    extends RefactorDesc(RefactorType.AddImport)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ExpandMatchCasesDesc(file: File, start: Int, end: Int)
    extends RefactorDesc(RefactorType.ExpandMatchCases)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait PatchOp {
  def start: Int
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class PatchInsert(
  start: Int,
  text: String
) extends PatchOp

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class PatchDelete(
  start: Int,
  end: Int
) extends PatchOp

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class PatchReplace(
  start: Int,
  end: Int,
  text: String
) extends PatchOp

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait EntityInfo extends RpcResponse {
  def name: String
  def members: List[EntityInfo]
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

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait SourceSymbol

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ObjectSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ClassSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object TraitSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object PackageSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ConstructorSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ImportedNameSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object TypeParamSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ParamSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object VarFieldSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ValFieldSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object OperatorFieldSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object VarSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ValSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object FunctionCallSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ImplicitConversionSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object ImplicitParamsSymbol extends SourceSymbol
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object DeprecatedSymbol extends SourceSymbol

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait PosNeeded
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object PosNeededNo extends PosNeeded
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object PosNeededAvail extends PosNeeded
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object PosNeededYes extends PosNeeded

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait SourcePosition extends RpcResponse
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class EmptySourcePosition() extends SourcePosition
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class OffsetSourcePosition(file: EnsimeFile, offset: Int)
    extends SourcePosition
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class LineSourcePosition(file: EnsimeFile, line: Int)
    extends SourcePosition

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class PositionHint(position: SourcePosition, preview: Option[String])
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SourcePositions(positions: List[PositionHint])
    extends RpcResponse

// See if `TypeInfo` can be used instead
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ClassInfo(scalaName: Option[String],
                           fqn: String,
                           declAs: DeclaredAs,
                           sourcePosition: Option[SourcePosition])

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class HierarchyInfo(ancestors: List[ClassInfo],
                               inheritors: List[ClassInfo])
    extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class PackageInfo(
  name: String,
  fullName: String,
  // n.b. members should be sorted by name for consistency
  members: List[EntityInfo]
) extends EntityInfo {
  require(members == members.sortBy(_.name), "members should be sorted by name")
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait SymbolSearchResult extends RpcResponse {
  def name: String
  def localName: String
  def declAs: DeclaredAs
  def pos: Option[SourcePosition]
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class TypeSearchResult(
  name: String,
  localName: String,
  declAs: DeclaredAs,
  pos: Option[SourcePosition]
) extends SymbolSearchResult

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class MethodSearchResult(
  name: String,
  localName: String,
  declAs: DeclaredAs,
  pos: Option[SourcePosition],
  ownerName: String
) extends SymbolSearchResult

// what is the point of these types?
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ImportSuggestions(symLists: List[List[SymbolSearchResult]])
    extends RpcResponse
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SymbolSearchResults(syms: List[SymbolSearchResult])
    extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SymbolDesignations(
  file: EnsimeFile,
  syms: List[SymbolDesignation]
) extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SymbolDesignation(
  start: Int,
  end: Int,
  symType: SourceSymbol
)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class SymbolInfo(
  name: String,
  localName: String,
  declPos: Option[SourcePosition],
  `type`: TypeInfo
) extends RpcResponse {
  def tpe = `type`
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class Op(
  op: String,
  description: String
)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class MethodBytecode(
  className: String,
  methodName: String,
  methodSignature: Option[String],
  byteCode: List[Op],
  startLine: Int,
  endLine: Int
)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class CompletionInfo(
  typeInfo: Option[TypeInfo],
  name: String,
  relevance: Int,
  toInsert: Option[String],
  isInfix: Boolean = false
) extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class CompletionInfoList(
  prefix: String,
  completions: List[CompletionInfo]
) extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class Breakpoint(file: EnsimeFile, line: Int) extends RpcResponse
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class BreakpointList(active: List[Breakpoint],
                                pending: List[Breakpoint])
    extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
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

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait TypeInfo extends EntityInfo {
  def name: String
  def declAs: DeclaredAs
  def fullName: String
  def typeArgs: List[TypeInfo]
  def members: List[EntityInfo]
  def pos: Option[SourcePosition]
  def typeParams: List[TypeInfo]

  final def declaredAs = declAs
  final def args       = typeArgs
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class BasicTypeInfo(
  name: String,
  declAs: DeclaredAs,
  fullName: String,
  typeArgs: List[TypeInfo],
  members: List[EntityInfo],
  pos: Option[SourcePosition],
  typeParams: List[TypeInfo]
) extends TypeInfo

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ArrowTypeInfo(
  name: String,
  fullName: String,
  resultType: TypeInfo,
  paramSections: List[ParamSectionInfo],
  typeParams: List[TypeInfo]
) extends TypeInfo {
  def declAs   = DeclaredAs.Nil
  def typeArgs = List.empty
  def members  = List.empty
  def pos      = None
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ParamSectionInfo(
  params: List[(String, TypeInfo)],
  isImplicit: Boolean
)

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class InterfaceInfo(
  `type`: TypeInfo,
  viaView: Option[String]
) extends RpcResponse {
  def tpe = `type`
}

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class FileRange(file: String, start: Int, end: Int)
    extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class EnsimeImplementation(
  name: String
)
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ConnectionInfo(
  pid: Option[Int] = None,
  implementation: EnsimeImplementation = EnsimeImplementation("ENSIME"),
  version: String = "1.9.6"
) extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait ImplicitInfo

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends ImplicitInfo

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ImplicitParamInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends ImplicitInfo

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class ImplicitInfos(infos: List[ImplicitInfo]) extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
sealed trait LegacyRawResponse extends RpcResponse
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object FalseResponse extends LegacyRawResponse
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
case object TrueResponse extends LegacyRawResponse
@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class StringResponse(text: String) extends LegacyRawResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class StructureView(view: List[StructureViewMember])
    extends RpcResponse

@deriving(JsReader, JsWriter, SexpReader, SexpWriter, Canon)
final case class StructureViewMember(
  keyword: String,
  name: String,
  position: SourcePosition,
  members: List[StructureViewMember]
)
