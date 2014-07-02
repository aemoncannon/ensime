import sbt._
import java.io._
import net.virtualvoid.sbt.graph.Plugin.graphSettings

organization := "org.ensime"

name := "ensime"

scalaVersion := "2.9.3"

version := "0.9.10-SNAPSHOT"

libraryDependencies <<= scalaVersion { scalaVersion => Seq(
  "org.apache.lucene"          %  "lucene-core"          % "3.5.0",
  "org.sonatype.tycho"         %  "org.eclipse.jdt.core" % "3.6.2.v_A76_R36x",
  "asm"                        %  "asm-commons"          % "3.3.1",
  "asm"                        %  "asm-util"             % "3.3.1",
  "com.googlecode.json-simple" %  "json-simple"          % "1.1.1" intransitive(),
  "org.scalatest"              %% "scalatest"            % "1.9.2" % "test",
  "org.scalariform"            %% "scalariform"          % "0.1.4",
  "org.scala-lang"             %  "scala-compiler"       % scalaVersion,
  "com.typesafe.akka" %% "akka-actor" % "2.0.5",
  "com.typesafe.akka" %% "akka-slf4j" % "2.0.5",
  "com.typesafe.akka" %% "akka-testkit" % "2.0.5" % "test",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "org.scala-refactoring"      %% "org.scala-refactoring.library" % "0.6.2"
)}

val JavaTools = List (
  sys.env.get("JDK_HOME").getOrElse(""),
  sys.env.get("JAVA_HOME").getOrElse(""),
  System.getProperty("java.home")
).map {
  n => new File(n + "/lib/tools.jar")
}.find(_.exists).getOrElse (
  throw new FileNotFoundException("tools.jar")
)

internalDependencyClasspath in Compile += { Attributed.blank(JavaTools) }

scalacOptions in Compile ++= Seq(
  "-encoding", "UTF-8", "-unchecked" //, "-Xfatal-warnings"
)

javacOptions in (Compile, compile) ++= Seq (
  "-source", "1.6", "-target", "1.6", "-Xlint:all", //"-Werror",
  "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
)

javacOptions in doc ++= Seq("-source", "1.6")

maxErrors := 1

graphSettings

scalariformSettings

licenses := Seq("BSD 3 Clause" -> url("http://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("http://github.com/ensime/ensime-server"))

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.contains("SNAP")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else                    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(
  "Sonatype Nexus Repository Manager", "oss.sonatype.org",
  sys.env.get("SONATYPE_USERNAME").getOrElse(""),
  sys.env.get("SONATYPE_PASSWORD").getOrElse("")
)
