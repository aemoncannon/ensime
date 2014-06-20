/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import sbt._
import Keys._
import sbt.Path
import IO._
import java.io.FileInputStream

object EnsimeBuild extends Build {

  private def doSh(str:String, cwd:Option[File]) = Process("sh" :: "-c" :: str :: Nil, cwd)
  private def doSh(str:String) = Process("sh" :: "-c" :: str :: Nil, None)

  private def file(str:String) = new File(str)

  private lazy val toolsJarCandidates: List[File] = {
    val jdkHome = Option(System.getenv("JAVA_HOME")).getOrElse("/tmp")
    val jreHome = new File(System.getProperty("java.home"))
    List[File](
      new File(jdkHome + "/lib/tools.jar"),
      new File(jreHome.getParent + "/lib/tools.jar"))
  }

  private lazy val toolsJar: Option[File] = {
    toolsJarCandidates.find(_.exists)
  }

  val root = Path(".")

  lazy val project = {
    Project(
      id = "ensime",
      base = file ("."),
      settings = Project.defaultSettings ++
      Seq(
        version := "0.9.8.11-SNAPSHOT",
        organization := "org.ensime",
        scalaVersion := "2.9.3",
        resolvers ++= Seq(
          Resolver.mavenLocal,
          Resolver.typesafeRepo("releases"),
          Resolver.sonatypeRepo("snapshots"),
          // are the following really needed?
          "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
          "Sonatype OSS Repository 2" at "https://oss.sonatype.org/content/groups/scala-tools/",
          "Sonatype OSS Snapshot Repository" at "https://oss.sonatype.org/content/repositories/snapshots",
          "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2",
          "repo.codahale.com" at "http://repo.codahale.com"
          )
        ,
        libraryDependencies  <++= (scalaVersion) { scalaVersion => Seq(
          "org.apache.lucene"          %  "lucene-core"          % "3.5.0",
          "org.sonatype.tycho"         %  "org.eclipse.jdt.core" % "3.6.2.v_A76_R36x",
          "asm"                        %  "asm"                  % "3.3.1",
          "asm"                        %  "asm-commons"          % "3.3.1",
          "asm"                        %  "asm-util"             % "3.3.1",
          "com.googlecode.json-simple" %  "json-simple"          % "1.1.1",
          "org.scalatest"              %% "scalatest"            % "1.9.2" % "test",
          "org.scalariform"            %% "scalariform"          % "0.1.4",
          "org.scala-lang"             %  "scala-compiler"       % scalaVersion,
          "org.scala-refactoring"      % "org.scala-refactoring.library_2.9.2" % "0.6.2"
        )},
        unmanagedClasspath in Compile ++= toolsJar.toList,
        scalacOptions ++= Seq("-g:vars","-deprecation"),
        exportJars := true,
        stageTask,
        distTask,
        releaseTask
      ))
  }

  val log = MainLogging.defaultScreen

  var stage = TaskKey[Unit]("stage",
    "Copy files into staging directory for a release.")
  lazy val stageTask:Setting[sbt.Task[Unit]] =
  stage <<= (
    dependencyClasspath in Runtime,
    exportedProducts in Runtime,
    scalaVersion) map { (depCP, exportedCP, scalaBuildVersion) =>

    val distDir = "dist_" + scalaBuildVersion

    delete(file(distDir))

    log.info("Copying runtime environment to ./" + distDir + "....")
    createDirectories(List(
      file(distDir),
      file(distDir + "/bin"),
      file(distDir + "/lib")))

    // Scalac components
    val scalaComponents = List("library", "compiler")

    val jars = scalaComponents map (c => "scala-" + c + ".jar")
    val scala = jars

    // Copy the emacs lisp to dist
    val elisp_base = root / "src" / "main" / "elisp"
    val elisp = ( elisp_base ** "*.el" ) +++ ( elisp_base ** "Makefile" )
    copy(elisp x flat(root / distDir ))

    // Copy the runtime jars
    val deps = (depCP ++ exportedCP).map(_.data)
    copy(deps x flat(root / distDir / "lib"))

    // Grab all jars..
    val cpLibs = (root / distDir / "lib" ** "*.jar").get.flatMap(
      _.relativeTo(root / distDir))

    def writeScript(bootclasspath:String, classpath:String, from:String, to:String){
      val tmplF = new File(from)
      val tmpl = read(tmplF)
      var s = tmpl.replace("<RUNTIME_CLASSPATH>", classpath)
      s = s.replace("<RUNTIME_BOOTCLASSPATH>", bootclasspath)
      val f = new File(to)
      write(f, s)
      f.setExecutable(true)
    }

    {
      val runtimeLibs = cpLibs ++ Seq("${JAVA_HOME}/lib/tools.jar")
      // Expand the server invocation script templates.
      def nix_wrap[T](cpEntries: Traversable[T]): String =
        "\"" + (cpEntries map (_.toString) mkString ":").replace("\\", "/") + "\""
      writeScript(
        nix_wrap(scala),
        nix_wrap(runtimeLibs),
        "./etc/scripts/server",
        "./" + distDir + "/bin/server")
    }

    {
      val runtimeLibs = cpLibs ++ Seq("%JAVA_HOME%/lib/tools.jar")
      def win_wrap[T](cpEntries: Traversable[T]): String = {
        def ensureAbsPath(p: String) = if ((p contains ":") || (p contains "%")) p else ("%~dp0\\..\\" + p)
        "\"" + (cpEntries map (_.toString) map (c => ensureAbsPath(c)) mkString ";").replace("/", "\\") + "\""
      }
      writeScript(
        win_wrap(scala),
        win_wrap(runtimeLibs),
        "./etc/scripts/server.bat",
        "./" + distDir + "/bin/server.bat")
    }

    copyFile(root / "README.md", root / distDir / "README.md")
    copyFile(root / "LICENSE", root / distDir / "LICENSE")

    val distCommon = "dist"
    delete(file(distCommon))
    log.info("Symlinking to ./" + distCommon + "....")
    doSh("ln -s  " + distDir + " " + distCommon)!!(log)
  }


  var dist = TaskKey[Unit]("dist", "Create the release package.")
  lazy val distTask:Setting[sbt.Task[Unit]] = dist := {
    println("The 'dist' task is deprecated. Use 'stage' to create release directory structure. Use 'release' to create the release archive.")
    None
  }


  var release = TaskKey[Unit]("release", "Create the release package and tag the current commit.")
  lazy val releaseTask:Setting[sbt.Task[Unit]] =
  release <<= (stage,version,scalaVersion) map {
    (_,version,scalaBuildVersion) =>

    val distDir = "dist_" + scalaBuildVersion
    val modName = "ensime_" + scalaBuildVersion + "-" + version
    val tagName = scalaBuildVersion + "-" + version

    val shallWeTag = false
    val tagArg = if(shallWeTag){ "-s" }else{ "" }
    doSh("git tag " + tagArg + " v" + tagName +
      " -m 'Tag for release " + modName + "'") !! (log)

    val initialDir = new File(".")
    val archiveFile = new File(initialDir,
      modName + ".tar.gz").getCanonicalPath
    withTemporaryDirectory{ f =>
      val releaseDir = new File(f.getAbsolutePath + "/" + modName)
      log.info("Copying ./" + distDir + " to temp directory: " + releaseDir)
      doSh("cp -r ./" + distDir + " " + releaseDir)!!(log)
      log.info("Compressing temp directory to " + archiveFile + "...")
      doSh("tar -pcvzf " + archiveFile + " " + modName, Some(f)) !! (log)
      None
    }
    None
  }
}
