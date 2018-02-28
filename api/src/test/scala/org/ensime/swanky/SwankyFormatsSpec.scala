// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.swanky

import java.io.File

import org.scalactic.source.Position
import org.scalatest._

import org.ensime.sexp._
import org.ensime.api._
import org.ensime.util.EscapingStringInterpolation

class SwankyFormatsSpec extends FlatSpec with Matchers {

  import EnsimeTestData._
  import EscapingStringInterpolation._

  import SexpReader.ops._
  import SexpWriter.ops._

  // copied from s-express:test to avoid a test->test dependency
  def assertFormat[T: SexpReader: SexpWriter](start: T, expect: Sexp)(
    implicit p: Position
  ): Unit = {
    val sexp      = start.toSexp
    val converted = sexp == expect // reduces noise in scalatest reporting
    assert(
      converted,
      s"\n${SexpCompactPrinter(sexp)}\nwas not\n${SexpCompactPrinter(expect)}"
    )
    expect.as[T] should be(start)
  }

  def roundtrip(value: RpcRequest, via: String)(implicit p: Position): Unit = {
    val enveloped = RpcRequestEnvelope(value, -1)
    assertFormat(enveloped, SexpParser(s"""(:req $via :call-id -1)"""))
  }

  def roundtrip(value: EnsimeServerMessage,
                via: String)(implicit p: Position): Unit = {
    val enveloped = RpcResponseEnvelope(None, value)
    assertFormat(enveloped, SexpParser(s"""(:payload $via)"""))
  }

  implicit def toFile(raw: RawFile): File = raw.file.toFile

  "SWANK Formats" should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      ":ensime-api-connection-info-req"
    )
  }

  it should "roundtrip RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """(:ensime-api-public-symbol-search-req (:keywords ("foo" "bar") :max-results 10))"""
    )

    roundtrip(
      ImportSuggestionsReq(Left(file1), 1, List("foo", "bar"), 10): RpcRequest,
      s"""(:ensime-api-import-suggestions-req (:file "$file1" :point 1 :names ("foo" "bar") :max-results 10))"""
    )
  }

  it should "roundtrip RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""(:ensime-api-remove-file-req (:file "$file1"))"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""(:ensime-api-typecheck-file-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(List(Left(file1), Left(file2))): RpcRequest,
      s"""(:ensime-api-typecheck-files-req (:files ("$file1" "$file2")))"""
    )

    roundtrip(
      TypecheckFilesReq(
        List(Right(SourceFileInfo(file1)),
             Right(SourceFileInfo(file2, Some("xxx"), None)))
      ): RpcRequest,
      s"""(:ensime-api-typecheck-files-req (:files ((:file "$file1") (:file "$file2" :contents "xxx"))))"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """:ensime-api-unload-all-req"""
    )

    roundtrip(
      DocUriAtPointReq(Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""(:ensime-api-doc-uri-at-point-req (:file "$file1" :point (:from 1 :to 10)))"""
    )

    roundtrip(
      DocUriAtPointReq(Right(SourceFileInfo(file1, None, Some(file2))),
                       OffsetRange(1, 10)): RpcRequest,
      s"""(:ensime-api-doc-uri-at-point-req (:file (:file "$file1" :contents-in "$file2") :point (:from 1 :to 10)))"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""(:ensime-api-completions-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :point 10 :max-results 100 :case-sens t))"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(sourceFileInfo, 100): RpcRequest,
      s"""(:ensime-api-uses-of-symbol-at-point-req (:file (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :point 100))"""
    )

    roundtrip(
      HierarchyOfTypeAtPointReq(sourceFileInfo, 100): RpcRequest,
      s"""(:ensime-api-hierarchy-of-type-at-point-req (:file (:file "$file1" :contents "{/* code here */}" :contents-in "$file2") :point 100))"""
    )

    roundtrip(
      TypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""(:ensime-api-type-at-point-req (:file "$file1" :range (:from 1 :to 100)))"""
    )

    roundtrip(
      SymbolAtPointReq(Left(file1), 101): RpcRequest,
      s"""(:ensime-api-symbol-at-point-req (:file "$file1" :point 101))"""
    )

    roundtrip(
      RefactorReq(1, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""(:ensime-api-refactor-req (:proc-id 1 :params (:ensime-api-rename-refactor-desc (:new-name "bar" :file "$file1" :start 1 :end 100))))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Left(file1),
        1,
        100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:ensime-api-symbol-designations-req (:file "$file1" :start 1 :end 100 :requested-types (:ensime-api-object-symbol :ensime-api-val-symbol)))"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Right(SourceFileInfo(file1, None, None)),
        1,
        100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""(:ensime-api-symbol-designations-req (:file (:file "$file1") :start 1 :end 100 :requested-types (:ensime-api-object-symbol :ensime-api-val-symbol)))"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""(:ensime-api-expand-selection-req (:file "$file1" :start 100 :end 200))"""
    )

    roundtrip(
      ImplicitInfoReq(Left(file1), OffsetRange(0, 123)),
      s"""(:ensime-api-implicit-info-req (:file "$file1" :range (:from 0 :to 123)))"""
    )

    roundtrip(
      StructureViewReq(sourceFileInfo): RpcRequest,
      s"""(:ensime-api-structure-view-req (:file-info (:file "$file1" :contents "{/* code here */}" :contents-in "$file2")))"""
    )

    roundtrip(
      UnloadFileReq(sourceFileInfo2): RpcRequest,
      s"""(:ensime-api-unload-file-req (:file-info (:file "$file1")))"""
    )
  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent,
      """(:ensime-api-send-background-message-event (:detail "ABCDEF" :code 1))"""
    )

    roundtrip(
      IndexerReadyEvent: EnsimeEvent,
      ":ensime-api-indexer-ready-event"
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeEvent,
      """(:ensime-api-new-scala-notes-event (:notes ((:file "foo.scala" :msg "testMsg" :severity :ensime-api-note-warn :beg 50 :end 55 :line 77 :col 5))))"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeEvent,
      ":ensime-api-clear-all-scala-notes-event"
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: Note,
      """(:ensime-api-note (:file "file1" :msg "note1" :severity :ensime-api-note-error :beg 23 :end 33 :line 19 :col 8))"""
    )

    roundtrip(
      completionInfo: CompletionInfo,
      """(:ensime-api-completion-info (:type-info (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :name "name" :relevance 90 :to-insert "BAZ"))"""
    )

    roundtrip(
      completionInfo2: CompletionInfo,
      """(:ensime-api-completion-info (:name "nam" :relevance 91 :is-infix t))"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList,
      """(:ensime-api-completion-info-list (:prefix "fooBar" :completions ((:type-info (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :name "name" :relevance 90 :to-insert "BAZ"))))"""
    )

    roundtrip(
      SymbolInfo("name", "localName", None, typeInfo): SymbolInfo,
      """(:ensime-api-symbol-info (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))))"""
    )

    roundtrip(
      NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo,
      """(:ensime-api-named-type-member-info (:name "typeX" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :decl-as :ensime-api-method))"""
    )

    roundtrip(
      entityInfo: EntityInfo,
      """(:ensime-api-arrow-type-info (:name "Arrow1" :full-name "example.Arrow1" :result-type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :param-sections ((:params ((:_1 "ABC" :_2 (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))))))))"""
    )

    roundtrip(
      entityInfoTypeParams: EntityInfo,
      s"""(:ensime-api-arrow-type-info (:name "Arrow1" :full-name "example.Arrow1" :result-type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :param-sections ((:params ((:_1 "ABC" :_2 (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")))))) :type-params ((:ensime-api-basic-type-info (:name "A" :decl-as :ensime-api-nil :full-name "example.Arrow1.A")) (:ensime-api-basic-type-info (:name "B" :decl-as :ensime-api-nil :full-name "example.Arrow1.B")))))"""
    )

    roundtrip(
      typeInfo: EntityInfo,
      """(:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))"""
    )

    roundtrip(
      packageInfo: EntityInfo,
      """(:ensime-api-package-info (:name "name" :full-name "fullName"))"""
    )

    roundtrip(
      interfaceInfo: InterfaceInfo,
      """(:ensime-api-interface-info (:type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")) :via-view "DEF"))"""
    )

    roundtrip(
      structureView: StructureView,
      s"""(:ensime-api-structure-view (:view ((:keyword "class" :name "StructureView" :position (:ensime-api-line-source-position (:file "$file1" :line 57))) (:keyword "object" :name "StructureView" :position (:ensime-api-line-source-position (:file "$file1" :line 59)) :members ((:keyword "type" :name "BasicType" :position (:ensime-api-offset-source-position (:file "$file1" :offset 456))))))))"""
    )
  }

  it should "roundtrip search related responses" in {
    roundtrip(
      SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults,
      s"""(:ensime-api-symbol-search-results (:syms ((:ensime-api-method-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-method :pos (:ensime-api-line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr")) (:ensime-api-type-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-trait :pos (:ensime-api-line-source-position (:file "$abd" :line 10)))))))"""
    )

    roundtrip(
      ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions,
      s"""(:ensime-api-import-suggestions (:sym-lists (((:ensime-api-method-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-method :pos (:ensime-api-line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr")) (:ensime-api-type-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-trait :pos (:ensime-api-line-source-position (:file "$abd" :line 10))))))))"""
    )

    roundtrip(
      methodSearchRes: SymbolSearchResult,
      s"""(:ensime-api-method-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-method :pos (:ensime-api-line-source-position (:file "$abd" :line 10)) :owner-name "ownerStr"))"""
    )

    roundtrip(
      typeSearchRes: SymbolSearchResult,
      s"""(:ensime-api-type-search-result (:name "abc" :local-name "a" :decl-as :ensime-api-trait :pos (:ensime-api-line-source-position (:file "$abd" :line 10))))"""
    )

    roundtrip(
      SourcePositions(
        PositionHint(sourcePos2, Some("{/* code here */}")) :: Nil
      ),
      s"""(:ensime-api-source-positions (:positions ((:position (:ensime-api-line-source-position (:file "$file1" :line 59)) :preview "{/* code here */}")))))"""
    )

    roundtrip(
      hierarchyInfo: HierarchyInfo,
      s"""(:ensime-api-hierarchy-info (:ancestors ((:fqn "java.lang.object" :decl-as :ensime-api-class)) :inheritors ((:scala-name "def.foo" :fqn "def$$foo" :decl-as :ensime-api-class :source-position (:ensime-api-line-source-position (:file "$file1" :line 59))))))"""
    )
  }

  it should "roundtrip ranges and semantic highlighting" in {
    roundtrip(
      FileRange("/abc", 7, 9): FileRange,
      """(:ensime-api-file-range (:file "/abc" :start 7 :end 9))"""
    )

    roundtrip(
      SymbolDesignations(
        symFile,
        List(
          SymbolDesignation(7, 9, VarFieldSymbol),
          SymbolDesignation(11, 22, ClassSymbol)
        )
      ): SymbolDesignations,
      s"""(:ensime-api-symbol-designations (:file "$symFile" :syms ((:start 7 :end 9 :sym-type :ensime-api-var-field-symbol) (:start 11 :end 22 :sym-type :ensime-api-class-symbol))))"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))): ImplicitInfos,
      """(:ensime-api-implicit-infos (:infos ((:ensime-api-implicit-conversion-info (:start 5 :end 6 :fun (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))))))))"""
    )

    roundtrip(
      ImplicitInfos(
        List(
          ImplicitParamInfo(5,
                            6,
                            symbolInfo,
                            List(symbolInfo, symbolInfo),
                            true)
        )
      ): ImplicitInfos,
      s"""(:ensime-api-implicit-infos (:infos ((:ensime-api-implicit-param-info (:start 5 :end 6 :fun (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))) :params ((:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1"))) (:name "name" :local-name "localName" :type (:ensime-api-basic-type-info (:name "type1" :decl-as :ensime-api-method :full-name "FOO.type1")))) :fun-is-implicit t)))))"""
    )
  }

  it should "roundtrip refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): RefactorFailure,
      """(:ensime-api-refactor-failure (:procedure-id 7 :reason "message" :status failure))"""
    )

    roundtrip(
      refactorDiffEffect: RefactorDiffEffect,
      s"""(:ensime-api-refactor-diff-effect (:procedure-id 9 :refactor-type :ensime-api-add-import :diff "$file2"))"""
    )

  }

  it should "roundtrip legacy raw response types" in {
    roundtrip(
      FalseResponse,
      ":ensime-api-false-response"
    )

    roundtrip(
      TrueResponse,
      ":ensime-api-true-response"
    )

    roundtrip(
      StringResponse("wibble"),
      """(:ensime-api-string-response (:text "wibble"))"""
    )

    roundtrip(
      VoidResponse,
      """:ensime-api-void-response"""
    )

  }

}
