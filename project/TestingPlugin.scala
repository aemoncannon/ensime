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

/**
 * For testing/ projects used in the integration tests.
 */
object TestingPlugin extends AutoPlugin {
  override def requires = CommonPlugin

  val autoImport = TestingPluginKeys
  import autoImport._

  override def buildSettings = Seq()
  override def projectSettings = Seq(
    testFrameworks in Test := Nil,
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile, doc) := Nil,
    publishLocal := {},
    publish := {},
    scalacOptions in (Compile, compile) := Nil,
    scalacOptions in (Test, compile) := Nil,
    libraryDependencies := Seq(
      "org.scala-lang" % "scala-library" % scalaVersion.value
    )
  )
}

object TestingPluginKeys {
  def testingProject(dir: String) =
    Project(dir.replace("/", "_"), file(dir))
      .enablePlugins(TestingPlugin)
      .disablePlugins(ProjectPlugin)
      .disablePlugins(ScalafixPlugin)
      .disablePlugins(ScalafmtPlugin)
}
