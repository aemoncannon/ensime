// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.actor._
import akka.testkit.TestActorRef
import org.ensime.api._
import org.ensime.fixture.SharedTestKitFixture
import org.ensime.util.EnsimeSpec
import org.ensime.util.FileUtils.toSourceFileInfo
import org.ensime.util.ensimefile.EnsimeFile
import org.ensime.util.file._

class AnalyzerManagerSpec extends EnsimeSpec with SharedTestKitFixture {

  "Analyzer Manager" should "aggregate multiple EnsimeServerErrors into one" in withTestKit {
    testKist =>
      import testKist._

      class DummyFileMissingAnalyzer extends Actor {
        override def receive: Receive = {
          case TypecheckFilesReq(files) =>
            val missingFilePaths = files.map { f =>
              "\"" + toSourceFileInfo(f).file + "\""
            }.mkString(",")
            sender ! EnsimeServerError(
              s"file(s): ${missingFilePaths} do not exist"
            )
        }
      }
      withTempDir(dir => {
        val module1 = dir / "module1"
        val module2 = dir / "module2"
        if (module1.mkdir() && module2.mkdir()) {
          var rawDir = RawFile(dir.toPath)
          implicit val dummyConfig: EnsimeConfig = EnsimeConfig(
            rawDir,
            rawDir,
            rawDir,
            null,
            null,
            Nil,
            Nil,
            List(
              EnsimeProject(EnsimeProjectId("module1", "compile"),
                            Nil,
                            Set(RawFile(module1.toPath)),
                            Set.empty,
                            Nil,
                            Nil,
                            Nil,
                            Nil,
                            Nil),
              EnsimeProject(EnsimeProjectId("module2", "compile"),
                            Nil,
                            Set(RawFile(module2.toPath)),
                            Set.empty,
                            Nil,
                            Nil,
                            Nil,
                            Nil,
                            Nil)
            )
          )
          val analyzerManager = TestActorRef(
            AnalyzerManager(TestActorRef[Broadcaster],
                            (module) => Props(new DummyFileMissingAnalyzer()))
          )
          val file1     = module1 / "missing1.scala"
          val file1Path = file1.getAbsolutePath
          val file2     = module2 / "missing2.scala"
          val file2Path = file2.getAbsolutePath

          analyzerManager ! TypecheckFilesReq(List(Left(file1), Left(file2)))
          val error = expectMsgType[EnsimeServerError].description
          error should include(
            s"""file(s): "${EnsimeFile(file1Path)}" do not exist"""
          )
          error should include(
            s"""file(s): "${EnsimeFile(file2Path)}" do not exist"""
          )
        }
      })

  }
}
