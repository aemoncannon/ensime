import java.io._
import scala.util.{ Properties, Try }
import scala.sys.process._

import sbt.Keys._
import sbt.{ IntegrationTest => It, _ }
import sbtassembly.AssemblyKeys._
import sbtassembly.{ AssemblyKeys, MergeStrategy, PathList }
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin, ScalafixPlugin.autoImport._

import org.ensime.EnsimePlugin.JdkDir
import org.ensime.EnsimeKeys._

import fommil.SensibleSettings._
import fommil.SonatypeKeys._

/** Settings shared between ensime-server and its test projects. */
object CommonPlugin extends AutoPlugin {
  override def requires = fommil.SensiblePlugin && fommil.SonatypePlugin
  override def trigger  = allRequirements

  override def buildSettings = Seq(
    scalaVersion := "2.12.6",
    organization := "org.ensime",
    ensimeIgnoreMissingDirectories := true,
    ensimeJavaFlags += "-Xmx4g",
    sonatypeGithost := (Github, "ensime", "ensime-server"),
    licenses := Seq(GPL3),
    startYear := Some(2010),
    concurrentRestrictions += Tags.limit(Scalafix, 2)
  )

  override def projectSettings = Seq(
    scalacOptions += "-Yrangepos",
    addCompilerPlugin(scalafixSemanticdb)
  )

}
