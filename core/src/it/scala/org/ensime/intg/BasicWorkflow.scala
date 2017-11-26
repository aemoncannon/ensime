// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import scala.concurrent.duration._

import org.ensime.api
import org.ensime.api.{ BasicTypeInfo => _, _ }
import org.ensime.core._
import org.ensime.fixture._
import org.ensime.indexer.FullyQualifiedName
import org.ensime.model.BasicTypeInfo
import org.ensime.util.EnsimeSpec
import EnsimeFile.Implicits.DefaultCharset
import org.ensime.util.file._

class BasicWorkflow
    extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with RefactoringHandlerTestUtils {

  val original = EnsimeConfigFixture.SimpleTestProject

  "ensime-server" should "open the simple test project" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit testkit =>
        withProject { (project, asyncHelper) =>
          import testkit._

          val sourceRoot   = scalaMain(config)
          val fooFile      = sourceRoot / "org/example/Foo.scala"
          val fooPath      = fooFile.toPath
          val fooFilePath  = fooFile.getAbsolutePath
          val barFile      = sourceRoot / "org/example/Bar.scala"
          val barPath      = barFile.toPath
          val testRoot     = scalaTest(config)
          val blooSpecFile = testRoot / "org/example/BlooSpec.scala"

          project ! TypecheckFilesReq(List(Left(blooSpecFile)))
          expectMsg(VoidResponse)

          project ! UnloadFileReq(SourceFileInfo(EnsimeFile(blooSpecFile)))
          expectMsg(VoidResponse)

          project ! TypecheckModule(
            EnsimeProjectId("testing_simple", "compile")
          )
          expectMsg(VoidResponse)
          all(asyncHelper.receiveN(2)) should matchPattern {
            case CompilerRestartedEvent =>
            case n: NewScalaNotesEvent  =>
          }

          project ! UnloadAllReq
          expectMsg(VoidResponse)
          expectMsg(VoidResponse)
          all(asyncHelper.receiveN(2)) should matchPattern {
            case CompilerRestartedEvent   =>
            case note: NewScalaNotesEvent =>
          }

          // trigger typeCheck
          project ! TypecheckFilesReq(List(Left(fooFile), Left(barFile)))
          expectMsg(VoidResponse)
          // Asking to typecheck mising file should report an error not kill system

          val missingFile     = sourceRoot / "missing.scala"
          val missingFilePath = missingFile.getAbsolutePath
          project ! TypecheckFilesReq(List(Left(missingFile)))
          expectMsg(EnsimeServerError(s"""file(s): "${EnsimeFile(
            missingFilePath
          )}" do not exist"""))

          //-----------------------------------------------------------------------------------------------
          // semantic highlighting
          project ! SymbolDesignationsReq(Left(fooFile),
                                          -1,
                                          299,
                                          SourceSymbol.allSymbols)
          val designations = expectMsgType[SymbolDesignations]
          designations.file match {
            case rf: RawFile     => rf.file.toFile shouldBe fooFile
            case af: ArchiveFile => ???
          }
          designations.syms should contain(
            SymbolDesignation(12, 19, PackageSymbol)
          )
          // expected Symbols
          // ((package 12 19) (package 8 11) (trait 40 43) (valField 69 70) (class 100 103) (param 125 126) (class 128 131) (param 133 134) (class 136 142) (operator 156 157) (param 154 155) (functionCall 160 166) (param 158 159) (valField 183 186) (class 193 199) (class 201 204) (valField 214 217) (class 224 227) (functionCall 232 239) (operator 250 251) (valField 256 257) (valField 252 255) (functionCall 261 268) (functionCall 273 283) (valField 269 272)))

          //-----------------------------------------------------------------------------------------------
          // symbolAtPoint
          project ! SymbolAtPointReq(Left(fooFile), 128)
          expectMsgType[SymbolInfo]

          project ! SymbolAtPointReq(Left(fooFile), 68) // scala.Int symbol
          val symInfo = expectMsgType[SymbolInfo]
          symInfo.`type`.pos should matchPattern {
            case Some(OffsetSourcePosition(_, _)) =>
          }

          val sourceFileLocation =
            symInfo.`type`.pos.get.asInstanceOf[OffsetSourcePosition].file
          project ! SymbolDesignationsReq(
            Right(SourceFileInfo(sourceFileLocation, None, None)),
            -1,
            9999,
            SourceSymbol.allSymbols
          )
          val symbolDesignations = expectMsgType[SymbolDesignations]
          symbolDesignations.syms should not be empty

          //-----------------------------------------------------------------------------------------------
          // public symbol search - java.io.File

          project ! PublicSymbolSearchReq(List("java", "io", "File"), 30)
          val javaSearchSymbol = expectMsgType[SymbolSearchResults]
          assert(javaSearchSymbol.syms.exists {
            case TypeSearchResult("java.io.File",
                                  "File",
                                  DeclaredAs.Class,
                                  Some(_)) =>
              true
            case _ => false
          })
          //-----------------------------------------------------------------------------------------------
          // public symbol search - scala.util.Random
          project ! PublicSymbolSearchReq(List("scala", "util", "Random"), 2)
          expectMsgPF() {
            case SymbolSearchResults(res) if res.collectFirst {
                  case TypeSearchResult("scala.util.Random",
                                        "Random",
                                        DeclaredAs.Class,
                                        Some(_)) =>
                    true
                }.isDefined =>
            // this is a pretty ropey test at the best of times
          }

          //-----------------------------------------------------------------------------------------------
          // documentation for type at point
          val intDocSig = DocSigPair(DocSig(DocFqn("scala", "Int"), None),
                                     DocSig(DocFqn("", "int"), None))

          // NOTE these are handled as multi-phase queries in requesthandler
          project ! DocUriAtPointReq(Left(fooFile), OffsetRange(128))
          expectMsg(Some(intDocSig))

          //-----------------------------------------------------------------------------------------------
          // uses of symbol at point

          project ! TypecheckFilesReq(List(Left(fooFile)))
          expectMsg(VoidResponse)

          val packageFile     = sourceRoot / "org/example/package.scala"
          val packageFilePath = packageFile.getAbsolutePath

          project ! FqnOfSymbolAtPointReq(SourceFileInfo(EnsimeFile(fooFile),
                                                         None,
                                                         None),
                                          119)
          var fqn = expectMsgType[FullyQualifiedName].fqnString

          project ! FindUsages(fqn) // point on testMethod
          expectMsgType[SourcePositions].positions should contain theSameElementsAs List(
            PositionHint(LineSourcePosition(EnsimeFile(fooFile), 17),
                         Some("println(foo.testMethod(7, \"seven\"))")),
            PositionHint(LineSourcePosition(EnsimeFile(packageFile), 7),
                         Some("new Foo.Foo().testMethod(1, \"\")"))
          )

          //-----------------------------------------------------------------------------------------------
          // tree of symbol at point
          project ! FqnOfTypeAtPointReq(SourceFileInfo(EnsimeFile(fooFile),
                                                       None,
                                                       None),
                                        56)
          fqn = expectMsgType[FullyQualifiedName].fqnString

          project ! FindHierarchy(fqn) // point on class Bar
          expectMsgType[HierarchyInfo] should matchPattern {
            case HierarchyInfo(
                List(ClassInfo(None, "java.lang.Object", DeclaredAs.Class, _)),
                List(ClassInfo(Some("org.example.Foo.Buz"),
                               "org.example.Foo$Buz",
                               DeclaredAs.Class,
                               _),
                     ClassInfo(Some("org.example.Foo.Foo"),
                               "org.example.Foo$Foo",
                               DeclaredAs.Class,
                               _))
                ) =>
          }

          // note that the line numbers appear to have been stripped from the
          // scala library classfiles, so offset/line comes out as zero unless
          // loaded by the pres compiler
          project ! SymbolAtPointReq(Left(fooFile), 276)
          expectMsgPF() {
            case SymbolInfo(
                "testMethod",
                "testMethod",
                Some(OffsetSourcePosition(RawFile(`fooPath`), 114)),
                ArrowTypeInfo(
                  "(Int, String) => Int",
                  "(scala.Int, java.lang.String) => scala.Int",
                  BasicTypeInfo("Int", DeclaredAs.Class, "scala.Int"),
                  List(
                    ParamSectionInfo(List(("i",
                                           BasicTypeInfo("Int",
                                                         DeclaredAs.Class,
                                                         "scala.Int")),
                                          (s,
                                           BasicTypeInfo("String",
                                                         DeclaredAs.Class,
                                                         "java.lang.String"))),
                                     false)
                  ),
                  Nil
                )
                ) =>
          }

          // M-.  external symbol
          project ! SymbolAtPointReq(Left(fooFile), 190)
          expectMsgPF() {
            case SymbolInfo("Map",
                            "Map",
                            Some(OffsetSourcePosition(_, _)),
                            BasicTypeInfo("Map",
                                          DeclaredAs.Object,
                                          "scala.collection.immutable.Map")) =>
          }

          project ! SymbolAtPointReq(Left(fooFile), 343)
          expectMsgPF() {
            case SymbolInfo("fn",
                            "fn",
                            Some(OffsetSourcePosition(RawFile(`fooPath`), 304)),
                            api.BasicTypeInfo(
                              "(String) => Int",
                              DeclaredAs.Trait,
                              "(java.lang.String) => scala.Int",
                              List(BasicTypeInfo("String",
                                                 DeclaredAs.Class,
                                                 "java.lang.String"),
                                   BasicTypeInfo("Int",
                                                 DeclaredAs.Class,
                                                 "scala.Int")),
                              Nil,
                              None,
                              Nil
                            )) =>
          }

          project ! SymbolAtPointReq(Left(barFile), 150)
          expectMsgPF() {
            case SymbolInfo(
                "apply",
                "apply",
                Some(OffsetSourcePosition(RawFile(`barPath`), 59)),
                ArrowTypeInfo(
                  "(String, Int) => Foo",
                  "(java.lang.String, scala.Int) => org.example.Bar.Foo",
                  BasicTypeInfo("Foo", DeclaredAs.Class, "org.example.Bar.Foo"),
                  List(
                    ParamSectionInfo(List(("bar",
                                           BasicTypeInfo("String",
                                                         DeclaredAs.Class,
                                                         "java.lang.String")),
                                          ("baz",
                                           BasicTypeInfo("Int",
                                                         DeclaredAs.Class,
                                                         "scala.Int"))),
                                     false)
                  ),
                  Nil
                )
                ) =>
          }

          project ! SymbolAtPointReq(Left(barFile), 193)
          expectMsgPF() {
            case SymbolInfo(
                "copy",
                "copy",
                Some(OffsetSourcePosition(RawFile(`barPath`), 59)),
                ArrowTypeInfo(
                  "(String, Int) => Foo",
                  "(java.lang.String, scala.Int) => org.example.Bar.Foo",
                  BasicTypeInfo("Foo", DeclaredAs.Class, "org.example.Bar.Foo"),
                  List(
                    ParamSectionInfo(List(("bar",
                                           BasicTypeInfo("String",
                                                         DeclaredAs.Class,
                                                         "java.lang.String")),
                                          ("baz",
                                           BasicTypeInfo("Int",
                                                         DeclaredAs.Class,
                                                         "scala.Int"))),
                                     false)
                  ),
                  Nil
                )
                ) =>
          }

          project ! SymbolAtPointReq(Left(fooFile), 645)
          expectMsgPF() {
            case SymbolInfo(
                "poly",
                "poly",
                Some(OffsetSourcePosition(RawFile(`fooPath`), 593)),
                ArrowTypeInfo(
                  "(A, B) => (A, B)",
                  "(org.example.WithPolyMethod.A, org.example.WithPolyMethod.B) => (org.example.WithPolyMethod.A, org.example.WithPolyMethod.B)",
                  api.BasicTypeInfo(
                    "(A, B)",
                    DeclaredAs.Class,
                    "(org.example.WithPolyMethod.A, org.example.WithPolyMethod.B)",
                    List(BasicTypeInfo("A",
                                       DeclaredAs.Nil,
                                       "org.example.WithPolyMethod.A"),
                         BasicTypeInfo("B",
                                       DeclaredAs.Nil,
                                       "org.example.WithPolyMethod.B")),
                    Nil,
                    None,
                    Nil
                  ),
                  List(
                    ParamSectionInfo(
                      List(("a",
                            BasicTypeInfo("A",
                                          DeclaredAs.Nil,
                                          "org.example.WithPolyMethod.A")),
                           ("b",
                            BasicTypeInfo("B",
                                          DeclaredAs.Nil,
                                          "org.example.WithPolyMethod.B"))),
                      false
                    )
                  ),
                  List(BasicTypeInfo("A",
                                     DeclaredAs.Nil,
                                     "org.example.WithPolyMethod.A"),
                       BasicTypeInfo("B",
                                     DeclaredAs.Nil,
                                     "org.example.WithPolyMethod.B"))
                )
                ) =>
          }

          // expand selection around "seven" in `foo.testMethod` call
          project ! ExpandSelectionReq(fooFile, 215, 215)
          val expandRange1 = expectMsgType[FileRange]
          expandRange1 shouldBe FileRange(fooFilePath, 214, 217)

          project ! ExpandSelectionReq(fooFile, 214, 217)
          val expandRange2 = expectMsgType[FileRange]
          expandRange2 shouldBe FileRange(fooFilePath, 210, 229)

          project ! RefactorReq(1234,
                                RenameRefactorDesc("bar", fooFile, 215, 215),
                                false)
          expectMsgPF() {
            case RefactorDiffEffect(1234, RefactorType.Rename, diff) =>
              val relevantExpectedPart = s"""|@@ -14,5 +14,5 @@
                                             |   val map = Map[String, Int]()
                                             |-  val foo = new Foo()
                                             |-  println("Hello, " + foo.x)
                                             |-  println(foo.testMethod(7, "seven"))
                                             |+  val bar = new Foo()
                                             |+  println("Hello, " + bar.x)
                                             |+  println(bar.testMethod(7, "seven"))
                                             | \n""".stripMargin
              val diffContents         = diff.canon.readString()
              val expectedContents =
                expectedDiffContent(fooFilePath, relevantExpectedPart)

              if (diffContents == expectedContents) true
              else
                fail(
                  s"Different diff content than expected. \n Actual content: '$diffContents' \n ExpectedRelevantContent: '$relevantExpectedPart'"
                )
          }

          project ! TypecheckFilesReq(List(Left(fooFile), Left(barFile)))
          expectMsg(VoidResponse)

          project ! RefactorReq(4321,
                                RenameRefactorDesc("Renamed", barFile, 30, 30),
                                false)
          expectMsgPF() {
            case RefactorDiffEffect(4321, RefactorType.Rename, diff) =>
              val renamedFile =
                new File(barFile.getPath.replace("Bar", "Renamed"))
              val barChanges     = s"""|@@ -1,13 +0,0 @@
                                   |-package org.example
                                   |-
                                   |-object Bar extends App {
                                   |-  case class Foo(bar: String, baz: Int)
                                   |-  object Bla {
                                   |-    val foo: Foo = Foo(
                                   |-      bar = "Bar",
                                   |-      baz = 123
                                   |-    )
                                   |-
                                   |-    val fooUpd = foo.copy(bar = foo.bar.reverse)
                                   |-  }
                                   |-}
                                   |""".stripMargin
              val fooChanges     = s"""|@@ -34,3 +34,3 @@
                                   |   List(1, 2, 3).head + 2
                                   |-  val x = Bar.Bla
                                   |+  val x = Renamed.Bla
                                   | }
                                   |""".stripMargin
              val renamedChanges = s"""|@@ -0,0 +1,13 @@
                                       |+package org.example
                                       |+
                                       |+object Renamed extends App {
                                       |+  case class Foo(bar: String, baz: Int)
                                       |+  object Bla {
                                       |+    val foo: Foo = Foo(
                                       |+      bar = "Bar",
                                       |+      baz = 123
                                       |+    )
                                       |+
                                       |+    val fooUpd = foo.copy(bar = foo.bar.reverse)
                                       |+  }
                                       |+}
                                       |""".stripMargin
              val changes = Seq(
                (barFile.getPath, DeleteFile, barChanges),
                (fooFile.getPath, ChangeContents, fooChanges),
                (renamedFile.getPath, CreateFile, renamedChanges)
              )
              val expectedDiff = expectedDiffContent(changes)
              val diffContent  = diff.canon.readString()
              diffContent should ===(expectedDiff)
          }

          val bazFile = sourceRoot / "org/example2/Baz.scala"
          val toBeUnloaded = SourceFileInfo(
            EnsimeFile(sourceRoot / "org/example2/ToBeUnloaded.scala")
          )
          val toBeUnloaded2 =
            SourceFileInfo(EnsimeFile(sourceRoot / "org/example/package.scala"))

          project ! TypecheckFilesReq(
            List(Left(bazFile), Right(toBeUnloaded), Right(toBeUnloaded2))
          )
          expectMsg(VoidResponse)

          project ! UnloadFileReq(toBeUnloaded)
          expectMsg(VoidResponse)
          project ! UnloadFileReq(toBeUnloaded2)
          expectMsg(VoidResponse)
          // file with warning has been unloaded
          // `NewScalaNotesEvent` should now not appear when typechecking `bazFile`

          // drain the async queue of scala note noise
          while (asyncHelper.msgAvailable) asyncHelper.receiveOne(1 second)

          project ! TypecheckFilesReq(List(Left(bazFile)))
          expectMsg(VoidResponse)

        // we're getting a spurious error here about acyclic deps
        // asyncHelper.expectNoMessage(3 seconds)
        }
      }
    }
  }
}
