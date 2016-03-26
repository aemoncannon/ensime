package org
package ensime
package ensimeserver

import sbt._
import sbtcatalysts.CatalystsPlugin.autoImport._

object Dependencies {

  val akkaVersion = "2.3.14"
  val guavaVersion = "19.0"
  val logbackVersion = "1.7.16"
  val luceneVersion = "4.7.2"
  val quasiquotesVersion = "2.0.1"
  val scalaModulesVersion = "1.0.4"
  val scalatestVersion = "2.2.6"
  val streamsVersion = "1.0"


  // Versions for libraries and packages
  // Package -> version
  val versions = Map[String, String](
    "akka"                        -> akkaVersion,
    "scalatest"                   -> scalatestVersion,
    "scalamock-scalatest-support" -> "3.2.2"
  )

  // library definitions and links to their versions
  // Note that one version may apply to more than one library.
  // Library name -> version key, org, library
  val libraries = Map[String, (String, String, String)](
    "akka-testkit"                -> ("akka"                        , "com.typesafe.akka" , "akka-testkit"),
    "akka-slf4j"                  -> ("akka"                        , "com.typesafe.akka" , "akka-slf4j"),
    "scalamock-scalatest-support" -> ("scalamock-scalatest-support" , "org.scalamock"     , "scalamock-scalatest-support")
  )

  // compiler plugins definitions and links to their versions
  // Note that one version may apply to more than one plugin.
  // Library name -> version key, org, librar, crossVersion
  val scalacPlugins = Map[String, (String, String, String, CrossVersion)](
  )

  val macroParadise = Seq(
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  )

  def shapeless(scalaVersion: String) = {
    if (scalaVersion.startsWith("2.10.")) macroParadise
    else Nil
  } :+ "com.chuusai" %% "shapeless" % "2.3.0"

  val logback = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.5",
    "org.slf4j" % "slf4j-api" % logbackVersion,
    "org.slf4j" % "jul-to-slf4j" % logbackVersion,
    "org.slf4j" % "jcl-over-slf4j" % logbackVersion
  )

  val guava = Seq(
    "com.google.guava" % "guava" % guavaVersion,
    "com.google.code.findbugs" % "jsr305" % "3.0.1" % "provided"
  )

  val commonsVfs2 = Seq(
    "org.apache.commons" % "commons-vfs2" % "2.0" exclude ("commons-logging", "commons-logging")
  )

}