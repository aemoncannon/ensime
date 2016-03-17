// Copyright 2016 Sam Halliday
// Licence: http://www.apache.org/licenses/LICENSE-2.0
import scala.util.{ Properties, Try }
import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform._
import org.ensime.Imports.EnsimeKeys

/**
 * A bunch of sensible defaults that fommil typically uses.
 *
 * TODO: integrate / contribute to the typelevel sbt plugin.
 */
object Sensible {

  lazy val settings = Seq(
    ivyLoggingLevel := UpdateLogging.Quiet,
    conflictManager := ConflictManager.strict,

    scalacOptions in Compile ++= Seq(
      "-encoding", "UTF-8",
      "-target:jvm-1.6",
      "-feature",
      "-deprecation",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-Xlint",
      "-Yinline-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      //"-Ywarn-numeric-widen", // noisy
      //"-Ywarn-value-discard", // will require a lot of work
      "-Xfuture"
    ) ++ {
        if (scalaVersion.value.startsWith("2.11")) Seq("-Ywarn-unused-import")
        else Nil
      } ++ {
        // fatal warnings can get in the way during the DEV cycle
        if (sys.env.contains("CI")) Seq("-Xfatal-warnings")
        else Nil
      },

    javaOptions := Seq("-Xss2m", "-XX:MaxPermSize=256m", "-Xms1g", "-Xmx1g"),

    javaOptions ++= Seq(
      "-XX:+UseConcMarkSweepGC",
      "-XX:+CMSIncrementalMode",
      "-Dfile.encoding=UTF8",
      // disabling shared memory gives a small performance boost to tests
      "-XX:+PerfDisableSharedMem"
    ),

    javacOptions in (Compile, compile) ++= Seq(
      "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),

    javaOptions in Test ++= Seq(
      "-Dlogback.configurationFile=../logback-test.xml"
    ),

    javacOptions in doc ++= Seq("-source", "1.6"),

    javaOptions in run ++= yourkitAgent,

    maxErrors := 1,
    fork := true,

    // 4 x 1GB = 4GB
    concurrentRestrictions in Global := Seq(Tags.limitAll(4)),

    EnsimeKeys.scalariform := ScalariformKeys.preferences.value
  ) ++ inConfig(Test)(testSettings) ++ scalariformSettings

  // TODO: scalariformSettingsWithIt generalised
  def testSettings = Seq(
    parallelExecution := true,

    // one JVM per test suite
    fork := true,
    testForkedParallel := true,
    testGrouping <<= (
      definedTests,
      baseDirectory,
      javaOptions,
      outputStrategy,
      envVars,
      javaHome,
      connectInput
    ).map { (tests, base, options, strategy, env, javaHomeDir, connectIn) =>
        val opts = ForkOptions(
          bootJars = Nil,
          javaHome = javaHomeDir,
          connectInput = connectIn,
          outputStrategy = strategy,
          runJVMOptions = options,
          workingDirectory = Some(base),
          envVars = env
        )
        tests.map { test =>
          Tests.Group(test.name, Seq(test), Tests.SubProcess(opts))
        }
      },

    testOptions ++= noColorIfEmacs,
    testFrameworks := Seq(TestFrameworks.ScalaTest, TestFrameworks.JUnit)
  )

  // e.g. YOURKIT_AGENT=/opt/yourkit/bin/linux-x86-64/libyjpagent.so
  val yourkitAgent = Properties.envOrNone("YOURKIT_AGENT").map { name =>
    val agent = file(name)
    require(agent.exists(), s"Yourkit agent specified ($agent) does not exist")
    Seq(s"-agentpath:${agent.getCanonicalPath}")
  }.getOrElse(Nil)

  // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
  def noColorIfEmacs =
    if (sys.env.get("INSIDE_EMACS").isDefined)
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oWF"))
    else
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oF"))

}
