package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.io.FileUtils
import org.ensime.api._
import org.ensime.fixture._
import org.scalatest.{ Matchers, WordSpec }
import org.ensime.util.file._

/**
 * Tries to simulate SBT clean/compile to stress test timing issues.
 *
 * (which also tests the file watchers).
 */
class CompileTimingTest extends WordSpec with Matchers
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.TimingTestProject

  "ensime-server" should {
    "handle an sbt clean" in {
      withEnsimeConfig { implicit config =>
        withTestKit { implicit testkit =>
          withProject { (project, asyncHelper) =>
            import testkit._

            val sourceRoot = scalaMain(config)

            val example = sourceRoot / "p1/Example.scala"

            val target = (mainTarget / "..").canon
            val targetBak = (target / "../scala-classes.bak").canon
            val exampleDiskInfo = SourceFileInfo(example, None, None)

            FileUtils.copyDirectory(target, targetBak)

            // simulate sbt clean https://github.com/sbt/sbt/issues/106
            project ! TypecheckFileReq(exampleDiskInfo)
            FileUtils.deleteDirectory(target)
            project ! TypecheckFileReq(exampleDiskInfo)

            // intentionally after the deletion to encourage races
            expectMsg(VoidResponse)
            expectMsg(VoidResponse)

            asyncHelper.receiveN(3) should contain theSameElementsAs (Seq(
              // note we only get two of these events, you might have expected 3
              FullTypeCheckCompleteEvent,
              FullTypeCheckCompleteEvent,
              CompilerRestartedEvent
            ))

            // simulate sbt compile
            project ! TypecheckFileReq(exampleDiskInfo)
            FileUtils.copyDirectory(targetBak, target)
            project ! TypecheckFileReq(exampleDiskInfo)

            expectMsg(VoidResponse)
            expectMsg(VoidResponse)

            asyncHelper.receiveN(3) should contain theSameElementsAs (Seq(
              FullTypeCheckCompleteEvent,
              FullTypeCheckCompleteEvent,
              CompilerRestartedEvent
            ))

            asyncHelper.expectNoMsg()
          }
        }
      }
    }
  }

}
