import java.io._
import scala.util.{ Properties, Try }
import scala.sys.process._

import sbt.Keys._
import sbt.{ IntegrationTest => It, _ }
import sbtassembly.AssemblyKeys._
import sbtassembly.{ AssemblyKeys, MergeStrategy, PathList }
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._

import org.ensime.EnsimePlugin.JdkDir
import org.ensime.EnsimeKeys._

import fommil.SensibleSettings._
import fommil.SonatypeKeys._

object ProjectPlugin extends AutoPlugin {
  override def requires = CommonPlugin
  override def trigger  = allRequirements

  val autoImport = ProjectPluginKeys
  import autoImport._

  override def projectSettings = Seq(
    transitiveClassifiers := {
      // reduces the download burden when running in CI
      val orig = transitiveClassifiers.value
      if (sys.env.contains("CI")) Nil else orig
    },
    // WORKAROUND https://issues.scala-lang.org/browse/SI-10157
    scalacOptions in (Compile, doc) -= "-Xfatal-warnings",
    scalacOptions in Compile -= "-Ywarn-value-discard",
    scalacOptions ++= Seq(
      "-language:_",
      "-unchecked",
      "-Xexperimental" // SAM types in 2.11
    ),
    scalacOptions ++= {
      val dir = (baseDirectory in ThisBuild).value / "project"
      Seq(
        s"-Xmacro-settings:deriving.targets=$dir/deriving-targets.conf",
        s"-Xmacro-settings:deriving.defaults=$dir/deriving-defaults.conf"
      )
    },
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    ),
    libraryDependencies ++= Seq(
      "com.github.mpilquist" %% "simulacrum"     % "0.11.0",
      "com.fommil"           %% "deriving-macro" % "0.9.0",
    ),
    excludeDependencies += ExclusionRule("stax", "stax-api"),
    dependencyOverrides ++= Seq(
      "com.typesafe.akka" %% "akka-actor"   % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
      "org.apache.lucene" % "lucene-core"   % luceneVersion
    ),
    // disabling shared memory gives a small performance boost to
    // tests but jvisualvm will no longer see the process.
    javaOptions += "-XX:+PerfDisableSharedMem",
    javaOptions ++= Seq("-Xms512m", "-Xmx512m"),
    // only recognised by 2.12.2+
    javaOptions += "-Dscala.classpath.closeZip=true",
    // print the table to optimise your own apps. VFS (and OrientDB)
    // are heavy on interning.
    javaOptions ++= Seq(
      //"-XX:+PrintStringTableStatistics",
      "-XX:StringTableSize=1000003",
      "-XX:+UnlockExperimentalVMOptions",
      "-XX:SymbolTableSize=1000003"
    )
  )
}

object ProjectPluginKeys {
  implicit class IntegrationTestOps(p: Project) {
    def enableIntegrationTests: Project =
      p.configs(It)
        .settings(
          inConfig(It)(
            Defaults.testSettings ++ sensibleTestSettings ++ Seq(
              javaOptions ++= Seq("-Xms1400m", "-Xmx1400m")
            )
          )
        )
  }

  lazy val JavaTools: File = JdkDir / "lib/tools.jar"

  val luceneVersion = "6.4.2" // 6.6 deprecates index time boosting
  val nettyVersion  = "4.1.17.Final"
  val akkaVersion   = "2.5.7"
  val orientVersion = "2.2.30"
}
