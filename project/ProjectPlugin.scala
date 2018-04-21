import java.io._
import scala.util.{ Properties, Try }
import scala.sys.process._

import sbt.Keys._
import sbt.{ IntegrationTest => It, _ }
import sbtassembly.AssemblyKeys._
import sbtassembly.{ AssemblyKeys, MergeStrategy, PathList }
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin, ScalafixPlugin.autoImport._
import org.scalafmt.sbt.ScalafmtPlugin, ScalafmtPlugin.autoImport._

import org.ensime.EnsimePlugin.JdkDir
import org.ensime.EnsimeKeys._

import fommil.SensibleSettings._
import fommil.SonatypeKeys._

object ProjectPlugin extends AutoPlugin {
  override def requires = CommonPlugin
  override def trigger  = allRequirements

  val autoImport = ProjectPluginKeys
  import autoImport._

  override def buildSettings =
    Seq(
      ensimeServerFindUsages := true // for integration tests
    )

  override def projectSettings =
    Seq(
      scalafmtConfig := Some(file("project/scalafmt.conf")),
      scalafixConfig := Some(file("project/scalafix.conf")),
      transitiveClassifiers := {
        // reduces the download burden when running in CI
        val orig = transitiveClassifiers.value
        if (sys.env.contains("CI")) Nil else orig
      },
      scalacOptions ++= extraScalacOptions(scalaVersion.value),
      // WORKAROUND https://issues.scala-lang.org/browse/SI-10157
      scalacOptions in (Compile, doc) -= "-Xfatal-warnings",
      scalacOptions in Compile -= "-Ywarn-value-discard",
      scalacOptions ++= Seq(
        "-language:_",
        "-unchecked",
        "-Xexperimental" // SAM types in 2.11
      ),
      MacroParadise,
      libraryDependencies ++= Seq(
        "com.github.mpilquist" %% "simulacrum"     % "0.12.0",
        "com.fommil"           %% "deriving-macro" % derivingVersion
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
      ),
      libraryDependencies ++= sensibleTestLibs(Test)
    ) ++ Deriving
}

object ProjectPluginKeys {
  implicit class IntegrationTestOps(p: Project) {
    def enableIntegrationTests: Project =
      p.configs(It)
        .settings(
          inConfig(It)(
            Defaults.testSettings ++ sensibleTestSettings ++ Seq(
              javaOptions ++= Seq("-Xms1400m", "-Xmx1400m"),
              libraryDependencies ++= sensibleTestLibs(It)
            )
          )
        )
  }

  val luceneVersion    = "6.4.2" // 6.6 deprecates index time boosting
  val nettyVersion     = "4.1.22.Final"
  val akkaVersion      = "2.5.11"
  val orientVersion    = "2.2.33"
  val shapelessVersion = "2.3.3"
  val derivingVersion  = "0.13.1"

  def MacroParadise =
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    )
  def KindProjector =
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
  def Deriving = Seq(
    addCompilerPlugin("com.fommil" %% "deriving-plugin" % derivingVersion),
    libraryDependencies += "com.fommil" %% "deriving-macro" % derivingVersion % "provided"
  )

  def extraScalacOptions(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 12)) => Seq("-Ywarn-unused:imports")
      //Seq("-Ywarn-unused:patvars,imports,privates,locals")
      case _ => Nil
    }

  def sensibleTestLibs(config: Configuration) =
    Seq(
      // janino 3.0.6 is not compatible and causes http://www.slf4j.org/codes.html#replay
      "org.codehaus.janino" % "janino" % "2.7.8" % config,
      "org.scalatest"       %% "scalatest" % "3.0.5" % config
    ) ++ logback.map(_      % config)

}
