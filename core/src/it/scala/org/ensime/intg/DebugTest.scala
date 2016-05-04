// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import java.io.File

import akka.testkit._
import java.io.InputStream
import java.util.Scanner
import org.ensime.api._
import org.ensime.core._
import org.ensime.fixture._
import org.ensime.util._
import org.ensime.util.file._
import org.scalatest.Matchers
import scala.concurrent.{ Await, Future, Promise }
import scala.util.{ Properties, Try }
import scala.concurrent.duration._

// must be refreshing as the tests don't clean up after themselves properly
class DebugTest extends EnsimeSpec
    with SharedEnsimeConfigFixture
    with SharedTestKitFixture
    with SharedProjectFixture
    with DebugTestUtils {

  val original = EnsimeConfigFixture.DebugTestProject.copy(
    javaLibs = Nil // no need to index the JRE
  )

  // CHIP/Rory - create a basic stepping test - not a clever one.
  // CHIP/Rory - create a set value test
  // CHIP/Rory - DebugToStringReq test

  "Debug - stepping" should "handle stepping into a for loop" taggedAs Debugger ignore {
    // this test is currently ignored - it fails beccause the debugger is stepping into the 'for' loop code
    // rather than the next 'logical step'
    withEnsimeConfig { implicit config =>
      withTestKit { implicit testkit =>
        withProject { (project, asyncHelper) =>
          implicit val p = (project, asyncHelper)
          withDebugSession(
            "stepping.ForComprehensionListString",
            "stepping/ForComprehensionListString.scala",
            9
          ) { (threadId, breakpointsFile) =>
              import testkit._

              checkTopStackFrame(threadId, "stepping.ForComprehensionListString$", "main", 9)
              project ! DebugNextReq(threadId)
              expectMsg(VoidResponse)

              checkTopStackFrame(threadId, "stepping.ForComprehensionListString$$anonfun$main$1", "apply", 10)
            }
        }
      }
    }
  }

  "Breakpoints" should "trigger/continue" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "breakpoints.Breakpoints",
          "breakpoints/Breakpoints.scala",
          32
        ) { (threadId, breakpointsFile) =>
            import testkit._

            // NOTE: Can encounter scala/Predef.scala if picking stack trace
            //       at arbitrary point
            project ! DebugBacktraceReq(threadId, 0, 3)
            expectMsgType[DebugBacktrace] should matchPattern {
              case DebugBacktrace(List(
                DebugStackFrame(0, List(), 0, "breakpoints.Breakpoints", "mainTest",
                  LineSourcePosition(`breakpointsFile`, 32), _),
                DebugStackFrame(1, List(
                  DebugStackLocal(0, "args", "Array(length = 0)[<EMPTY>]", "java.lang.String[]")
                  ), 1, "breakpoints.Breakpoints$", "main",
                  LineSourcePosition(`breakpointsFile`, 42), _),
                DebugStackFrame(2, List(), 1, "breakpoints.Breakpoints", "main",
                  LineSourcePosition(`breakpointsFile`, _), _)
                ), `threadId`, "main") =>
            }

            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            project ! DebugSetBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 11))

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 13))

            project ! DebugClearBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 13))

            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            project ! DebugClearBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 11))

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 11))

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)
          }
      }
    }
  }

  it should "list/clear" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "breakpoints.Breakpoints",
          "breakpoints/Breakpoints.scala",
          32
        ) {
            case (threadId, breakpointsFile) =>
              import testkit._

              project ! DebugListBreakpointsReq
              expectMsgType[BreakpointList] should matchPattern {
                case BreakpointList(Nil, Nil) =>
              }

              // break in main
              project ! DebugSetBreakReq(breakpointsFile, 11)
              expectMsg(TrueResponse)
              project ! DebugSetBreakReq(breakpointsFile, 13)
              expectMsg(TrueResponse)

              // breakpoints should now be active
              project ! DebugListBreakpointsReq
              inside(expectMsgType[BreakpointList]) {
                case BreakpointList(activeBreakpoints, pendingBreakpoints) =>
                  activeBreakpoints should contain theSameElementsAs Set(
                    Breakpoint(breakpointsFile, 11), Breakpoint(breakpointsFile, 13)
                  )
                  pendingBreakpoints shouldBe empty
              }

              // check clear works again
              project ! DebugClearAllBreaksReq
              expectMsg(TrueResponse)
              project ! DebugListBreakpointsReq
              expectMsgType[BreakpointList] should matchPattern {
                case BreakpointList(Nil, Nil) =>
              }
          }
      }
    }
  }

  // starting up a debug session for each variable is unneeded and wasteful of test time.
  // this approach means that there is one test method, but it still explores all of the paths.
  "Debug Inspect variables" should "inspect variables" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "debug.Variables",
          "debug/Variables.scala",
          21
        ) { (threadId, variablesFile) =>
            // boolean local
            getVariableValue(threadId, "a") should matchPattern {
              case DebugPrimitiveValue("true", "boolean") =>
            }

            // char local
            getVariableValue(threadId, "b") should matchPattern {
              case DebugPrimitiveValue("'c'", "char") =>
            }

            // short local
            getVariableValue(threadId, "c") should matchPattern {
              case DebugPrimitiveValue("3", "short") =>
            }

            // int local
            getVariableValue(threadId, "d") should matchPattern {
              case DebugPrimitiveValue("4", "int") =>
            }

            // long local
            getVariableValue(threadId, "e") should matchPattern {
              case DebugPrimitiveValue("5", "long") =>
            }

            // float local
            getVariableValue(threadId, "f") should matchPattern {
              case DebugPrimitiveValue("1.0", "float") =>
            }

            // double local
            getVariableValue(threadId, "g") should matchPattern {
              case DebugPrimitiveValue("2.0", "double") =>
            }

            // String local
            inside(getVariableValue(threadId, "h")) {
              case DebugStringInstance("\"test\"", debugFields, "java.lang.String", _) =>
                exactly(1, debugFields) should matchPattern {
                  case DebugClassField(_, "value", "char[]", "Array(length = 4)['t','e','s',...]") =>
                }
            }

            // primitive array local
            getVariableValue(threadId, "i") should matchPattern {
              case DebugArrayInstance(3, "int[]", "int", _) =>
            }

            // type local
            inside(getVariableValue(threadId, "j")) {
              case DebugObjectInstance(summary, debugFields, "scala.collection.immutable.$colon$colon", _) =>
                summary should startWith("Instance of scala.collection.immutable.$colon$colon")
                exactly(1, debugFields) should matchPattern {
                  case DebugClassField(_, head, "java.lang.Object", summary) if (
                    (head == "head" || head == "scala$collection$immutable$$colon$colon$$hd") &&
                    summary.startsWith("Instance of java.lang.Integer")
                  ) =>
                }
            }

            // object array local
            getVariableValue(threadId, "k") should matchPattern {
              case DebugArrayInstance(3, "java.lang.Object[]", "java.lang.Object", _) =>
            }
          }
      }
    }
  }

  "Debug backtrace" should "generate backtrace" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "debug.Backtrace",
          "debug/Backtrace.scala",
          13
        ) { (threadId, f) =>
            import testkit._
            project ! DebugBacktraceReq(threadId, 0, 20)
            val backTrace = expectMsgType[DebugBacktrace]
            // just some sanity assertions
            assert(backTrace.frames.forall(_.className.startsWith("debug.Backtrace")))
            assert(backTrace.frames.forall(_.pcLocation.file.toString.endsWith("Backtrace.scala")))
          }
      }
    }
  }
}

trait DebugTestUtils {
  this: ProjectFixture with Matchers with EnsimeConfigFixture =>

  /**
   * Launches a new JVM using the given class name as the entrypoint. Pauses
   * the JVM at the specified source path and line.
   *
   * @param className containing the main method
   * @param fileName to place the breakpoint
   * @param breakLine where to start the session in the fileName
   * @param func The test code to execute (gives the debug thread id of the main thread and file handle)
   */
  def withDebugSession(
    className: String,
    fileName: String,
    breakLine: Int
  )(
    func: (DebugThreadId, File) => Any
  )(
    implicit
    config: EnsimeConfig,
    testkit: TestKitFix,
    // don't take an implicit TestActorRef or it steals the implicit sender
    p: (TestActorRef[Project], TestProbe)
  ): Any = {
    import testkit._
    val resolvedFile = scalaMain(config) / fileName
    val project = p._1
    val asyncHelper = p._2

    project ! DebugSetBreakReq(resolvedFile, breakLine)
    expectMsg(TrueResponse)

    val vm = VMStarter(config, className)
    Await.result(vm._4, (5 seconds).dilated)

    project ! DebugAttachReq(vm._2, vm._3.toString)

    expectMsg(DebugVmSuccess())

    asyncHelper.expectMsg(DebugVMStartEvent)

    val gotOnStartup = asyncHelper.expectMsgType[EnsimeServerMessage]
    // weird! we sometimes see a duplicate break event instantly, not really expected
    val additionalOnStartup = Try(asyncHelper.expectMsgType[EnsimeServerMessage](1 second)).toOption.toSeq
    // but it doesn't always come through

    val allEvents = gotOnStartup +: additionalOnStartup
    val threadId = allEvents.flatMap {
      case DebugBreakEvent(foundThreadId, "main", `resolvedFile`, `breakLine`) =>
        List(foundThreadId)
      case _ =>
        Nil
    }.headOption.getOrElse(fail("Cannot work out main threadId"))

    project ! DebugClearBreakReq(resolvedFile, breakLine)
    expectMsg(TrueResponse)

    try {
      func(threadId, resolvedFile)
    } finally {
      try {
        project ! DebugClearAllBreaksReq
        expectMsg(TrueResponse)
        // no way to await the stopped condition so we let the app run
        // its course on the main thread
        // CHIP/Rory Determine if there is any point doing this action at all.
//        project ! DebugContinueReq(threadId)
//        expectMsg(TrueResponse)
        project ! DebugStopReq
        expectMsgPF() {
          case TrueResponse =>
          case FalseResponse => // windows does this sometimes
        }
      } finally {
        vm._1.destroy()
      }
    }
  }

  def getVariableValue(threadId: DebugThreadId, variableName: String)(implicit testkit: TestKitFix, p: (TestActorRef[Project], TestProbe)): DebugValue = {
    import testkit._
    val project = p._1
    project ! DebugLocateNameReq(threadId, variableName)
    val vLoc = expectMsgType[DebugLocation]

    project ! DebugValueReq(vLoc)
    expectMsgType[DebugValue]
  }

  def checkTopStackFrame(threadId: DebugThreadId, className: String, method: String, line: Int)(implicit testkit: TestKitFix, p: (TestActorRef[Project], TestProbe)): Unit = {
    import testkit._
    val project = p._1

    project ! DebugBacktraceReq(threadId, 0, 1)
    expectMsgType[DebugBacktrace] should matchPattern {
      case DebugBacktrace(List(DebugStackFrame(0, _, 1, `className`, `method`,
        LineSourcePosition(_, `line`), _)),
        `threadId`, "main") =>
    }
  }
}

// only for test because only the build tool really knows how to launch the JVM
object VMStarter extends SLF4JLogging {

  def logLines(src: InputStream): Future[Unit] = {
    val promise = Promise[Unit]()
    new Thread(new Runnable() {
      override def run(): Unit = {
        val sc = new Scanner(src)
        while (sc.hasNextLine) {
          if (!promise.isCompleted) promise.trySuccess(())
          log.info("DEBUGGING_PROCESS:" + sc.nextLine())
        }
      }
    }).start()
    promise.future
  }

  def java: String =
    if (Properties.isWin) Properties.javaHome + """\bin\javaw.exe"""
    else Properties.javaHome + "/bin/java"

  def apply(config: EnsimeConfig, clazz: String): (Process, String, Int, Future[Unit]) = {
    import collection.JavaConverters._

    // would be nice to have ephemeral debug ports
    val port = 5000 + scala.util.Random.nextInt(1000)

    val classpath = (config.compileClasspath ++ config.targetClasspath).mkString(File.pathSeparator)
    val args = Seq(
      java,
      "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=" + port,
      "-Xms128m", "-Xmx128m",
      "-classpath", classpath,
      clazz
    )

    val process = new ProcessBuilder(
      args.asJava
    ).redirectErrorStream(true).start()

    val logging = logLines(process.getInputStream)

    (process, "127.0.0.1", port, logging)
  }

}
