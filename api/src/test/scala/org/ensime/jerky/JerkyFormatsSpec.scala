// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.jerky

import java.io.File

import org.scalatest._
import org.scalactic.source.Position

import org.ensime.api._
import org.ensime.util.EscapingStringInterpolation

class JerkyFormatsSpec
    extends FlatSpec
    with Matchers
    with SprayJsonTestSupport {

  import EnsimeTestData._
  import EscapingStringInterpolation._

  "Jerk Formats" should "roundtrip request envelopes" in {
    roundtrip(
      RpcRequestEnvelope(ConnectionInfoReq, 13),
      """{"callId":13,"req":{"typehint":"ConnectionInfoReq"}}"""
    )
  }

  it should "roundtrip RPC response envelopes" in {
    roundtrip(
      RpcResponseEnvelope(Some(13), sourcePos3),
      """{"callId":13,"payload":{"typehint":"EmptySourcePosition"}}"""
    )
  }

  it should "roundtrip async response envelopes" in {
    roundtrip(
      RpcResponseEnvelope(None, SendBackgroundMessageEvent("ABCDEF", 1)),
      """{"payload":{"typehint":"SendBackgroundMessageEvent","detail":"ABCDEF","code":1}}"""
    )
  }

  def roundtrip(value: RpcRequest, via: String)(implicit p: Position): Unit = {
    val enveloped = RpcRequestEnvelope(value, -1)
    roundtrip(enveloped, s"""{"req":$via, "callId":-1}""")
  }

  def roundtrip(value: EnsimeServerMessage,
                via: String)(implicit p: Position): Unit = {
    val enveloped = RpcResponseEnvelope(None, value)
    roundtrip(enveloped, s"""{"payload":$via}""")
  }

  implicit def toFile(raw: RawFile): File = raw.file.toFile

  it should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      """{"typehint":"ConnectionInfoReq"}"""
    )
  }

  it should "unmarshal RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """{"typehint":"PublicSymbolSearchReq","keywords":["foo","bar"],"maxResults":10}"""
    )

    roundtrip(
      ImportSuggestionsReq(Left(file1), 1, List("foo", "bar"), 10): RpcRequest,
      s"""{"point":1,"maxResults":10,"names":["foo","bar"],"typehint":"ImportSuggestionsReq","file":"$file1"}"""
    )
  }

  it should "unmarshal RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""{"typehint":"RemoveFileReq","file":"$file1"}"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"TypecheckFileReq","fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

    roundtrip(
      TypecheckFilesReq(List(Left(file1), Left(file2))): RpcRequest,
      s"""{"typehint":"TypecheckFilesReq","files":["$file1","$file2"]}"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """{"typehint":"UnloadAllReq"}"""
    )

    roundtrip(
      DocUriAtPointReq(Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""{"typehint":"DocUriAtPointReq","file":"$file1","point":{"from":1,"to":10}}"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""{"point":10,"maxResults":100,"typehint":"CompletionsReq","caseSens":true,"fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"},"reload":false}"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(sourceFileInfo, 100): RpcRequest,
      s"""{"typehint":"UsesOfSymbolAtPointReq","file":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"},"point":100}"""
    )

    roundtrip(
      HierarchyOfTypeAtPointReq(sourceFileInfo, 100): RpcRequest,
      s"""{"typehint":"HierarchyOfTypeAtPointReq","file":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"},"point":100}"""
    )

    roundtrip(
      TypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""{"typehint":"TypeAtPointReq","file":"$file1","range":{"from":1,"to":100}}"""
    )

    roundtrip(
      SymbolAtPointReq(Left(file1), 101): RpcRequest,
      s"""{"typehint":"SymbolAtPointReq","file":"$file1","point":101}"""
    )

    roundtrip(
      RefactorReq(1, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""{"procId":1,"params":{"newName":"bar","typehint":"RenameRefactorDesc","end":100,"file":"$file1","start":1},"typehint":"RefactorReq","interactive":false}"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Left(file1),
        1,
        100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""{"requestedTypes":[{"typehint":"ObjectSymbol"},{"typehint":"ValSymbol"}],"typehint":"SymbolDesignationsReq","end":100,"file":"$file1","start":1}"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Right(SourceFileInfo(file1, None, None)),
        1,
        100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""{"requestedTypes":[{"typehint":"ObjectSymbol"},{"typehint":"ValSymbol"}],"typehint":"SymbolDesignationsReq","file":{"file":"$file1"}, "end":100,"start":1}"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""{"typehint":"ExpandSelectionReq","file":"$file1","start":100,"end":200}"""
    )

    roundtrip(
      ImplicitInfoReq(Left(file1), OffsetRange(0, 123)): RpcRequest,
      s"""{"typehint":"ImplicitInfoReq","file":"$file1","range":{"from":0,"to":123}}"""
    )

    roundtrip(
      StructureViewReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"StructureViewReq","fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

    roundtrip(
      UnloadFileReq(sourceFileInfo2): RpcRequest,
      s"""{"typehint":"UnloadFileReq","fileInfo":{"file":"$file1"}}"""
    )
  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeServerMessage,
      """{"typehint":"SendBackgroundMessageEvent","detail":"ABCDEF","code":1}"""
    )

    roundtrip(
      IndexerReadyEvent: EnsimeServerMessage,
      """{"typehint":"IndexerReadyEvent"}"""
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeServerMessage,
      """{"typehint":"NewScalaNotesEvent","isFull":false,"notes":[{"beg":50,"line":77,"col":5,"end":55,"file":"foo.scala","msg":"testMsg","severity":{"typehint":"NoteWarn"}}]}"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeServerMessage,
      """{"typehint":"ClearAllScalaNotesEvent"}"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: EnsimeServerMessage,
      """{"typehint":"Note","beg":23,"line":19,"col":8,"end":33,"file":"file1","msg":"note1","severity":{"typehint":"NoteError"}}"""
    )

    roundtrip(
      completionInfo: EnsimeServerMessage,
      """{"name":"name","typeInfo":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"typehint":"CompletionInfo","relevance":90,"isInfix":false,"toInsert":"BAZ"}"""
    )

    roundtrip(
      completionInfo2: EnsimeServerMessage,
      """{"typehint":"CompletionInfo","name":"nam","relevance":91,"isInfix":true}"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): EnsimeServerMessage,
      """{"typehint":"CompletionInfoList","prefix":"fooBar","completions":[{"typeInfo":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"name":"name","relevance":90,"isInfix":false,"toInsert":"BAZ"}]}"""
    )

    roundtrip(
      new SymbolInfo("name", "localName", None, typeInfo): EnsimeServerMessage,
      """{"typehint":"SymbolInfo","name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}}"""
    )

    roundtrip(
      new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EnsimeServerMessage,
      """{"typehint":"NamedTypeMemberInfo","name":"typeX","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      entityInfo: EnsimeServerMessage,
      """{"resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"name":"Arrow1","fullName":"example.Arrow1","paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}],"typehint":"ArrowTypeInfo","typeParams":[]}"""
    )

    roundtrip(
      entityInfoTypeParams: EnsimeServerMessage,
      """{"resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"name":"Arrow1","fullName":"example.Arrow1","paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}],"typehint":"ArrowTypeInfo","typeParams":[{"name":"A","fullName":"example.Arrow1.A","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Nil"}},{"name":"B","fullName":"example.Arrow1.B","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Nil"}}]}"""
    )

    roundtrip(
      typeInfo: EnsimeServerMessage,
      """{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      packageInfo: EnsimeServerMessage,
      """{"typehint":"PackageInfo","name":"name","fullName":"fullName","members":[]}"""
    )

    roundtrip(
      interfaceInfo: EnsimeServerMessage,
      """{"typehint":"InterfaceInfo","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}"""
    )
  }

  it should "support search related responses" in {
    roundtrip(
      SymbolSearchResults(List(methodSearchRes, typeSearchRes)): EnsimeServerMessage,
      s"""{"typehint":"SymbolSearchResults","syms":[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]}"""
    )

    roundtrip(
      ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): EnsimeServerMessage,
      s"""{"typehint":"ImportSuggestions","symLists":[[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]]}"""
    )

    roundtrip(
      methodSearchRes: EnsimeServerMessage,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      typeSearchRes: EnsimeServerMessage,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}"""
    )

    roundtrip(
      SourcePositions(
        PositionHint(sourcePos2, Some("{/* code here */}")) :: Nil
      ): EnsimeServerMessage,
      s"""{"typehint":"SourcePositions","positions":[{"position":{"typehint":"LineSourcePosition","file":"$file1","line":59},"preview":"{/* code here */}"}]}"""
    )

    roundtrip(
      hierarchyInfo: EnsimeServerMessage,
      s"""{"typehint":"HierarchyInfo","ancestors":[{"fqn":"java.lang.object","declAs":{"typehint":"Class"}}],"inheritors":[{"scalaName":"def.foo","fqn":"def$$foo","declAs":{"typehint":"Class"},"sourcePosition":{"typehint":"LineSourcePosition","file":"$file1","line":59}}]}"""
    )
  }

  it should "support ranges and semantic highlighting" in {
    roundtrip(
      FileRange("/abc", 7, 9): EnsimeServerMessage,
      s"""{"typehint":"FileRange","file":"/abc","start":7,"end":9}"""
    )

    roundtrip(
      SymbolDesignations(
        symFile,
        List(
          SymbolDesignation(7, 9, VarFieldSymbol),
          SymbolDesignation(11, 22, ClassSymbol)
        )
      ): EnsimeServerMessage,
      s"""{"typehint":"SymbolDesignations","file":"$symFile","syms":[{"start":7,"end":9,"symType":{"typehint":"VarFieldSymbol"}},{"start":11,"end":22,"symType":{"typehint":"ClassSymbol"}}]}"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))): EnsimeServerMessage,
      """{"typehint":"ImplicitInfos","infos":[{"typehint":"ImplicitConversionInfo","start":5,"end":6,"fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}}}]}"""
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
      ): EnsimeServerMessage,
      """{"typehint":"ImplicitInfos","infos":[{"params":[{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}},{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}}],"typehint":"ImplicitParamInfo","fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeParams":[],"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}},"funIsImplicit":true,"end":6,"start":5}]}"""
    )

  }

  it should "support refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): EnsimeServerMessage,
      """{"typehint":"RefactorFailure","procedureId":7,"reason":"message","status":"failure"}"""
    )

    roundtrip(
      refactorDiffEffect: EnsimeServerMessage,
      s"""{"typehint":"RefactorDiffEffect","procedureId":9,"refactorType":{"typehint":"AddImport"},"diff":"$file2"}"""
    )

  }

  it should "support legacy raw response types" in {
    roundtrip(
      FalseResponse: EnsimeServerMessage,
      """{"typehint":"FalseResponse"}"""
    )

    roundtrip(
      TrueResponse: EnsimeServerMessage,
      """{"typehint":"TrueResponse"}"""
    )

    roundtrip(
      StringResponse("wibble"): EnsimeServerMessage,
      """{"typehint":"StringResponse","text":"wibble"}"""
    )

    roundtrip(
      VoidResponse: EnsimeServerMessage,
      """{"typehint":"VoidResponse"}"""
    )
  }
}
