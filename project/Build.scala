import sbt._
import sbt.IO._
import sbt.Logger._

import Keys._

import Path.relativeTo
import Path.richFile

import java.net.URLClassLoader

object BuildSettings {
  val buildOrganization = "ensime"
  val buildScalaVersion = "2.9.0-1"
  val buildVersion = "0.5.0"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    version      := buildVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
  )
}

// Shell prompt which show the current project, 
// git branch and build version
object ShellPrompt {
 
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  
  val current = """\*\s+(\w+)""".r
  
  def gitBranches = ("git branch --no-color" lines_! devnull mkString)
  
  val buildShellPrompt = { 
    (state: State) => {
      val currBranch = 
        current findFirstMatchIn gitBranches map (_ group(1)) getOrElse "-"
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
 
}

object Resolvers {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val jbossRepo = "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2"

  val sunrepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
  val sunrepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish" 
  val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"

  val oracleResolvers = Seq (sunrepo, sunrepoGF, oraclerepo)

  val scalaResolvers = Seq (scalaToolsSnapshots, jbossRepo)
}

object Dependencies {
  val scalac = "org.scala-lang" % "scala-compiler" % "2.9.0-1" % "compile;runtime;test"
  val ant = "org.apache.ant" % "ant" % "1.8.1" % "compile;runtime;test"
  val ivy = "org.apache.ivy" % "ivy" % "2.1.0" % "compile;runtime;test"
  val maven = "org.apache.maven" % "maven-ant-tasks" % "2.1.0" % "compile;runtime;test"
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val jdt = "org.eclipse.jdt" % "core" % "3.4.2.v_883_R34x" % "compile;runtime;test"
  val scalariform = "org.scalariform" % "scalariform_2.9.0-SNAPSHOT" % "0.0.9" % "compile;runtime;test"
//  val refactoring = "org.scala-refactoring" % "org.scala-refactoring.library" % "0.2.0-SNAPSHOT"%"compile;runtime;test"

  val asm = "asm" % "asm" % "3.2"
  val asmCommons = "asm" % "asm-commons" % "3.2"
}

object EnsimeBuild extends Build {

  implicit def toFile(name: String) = file(name)

  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq (
    scalac,
    ant,
    ivy,
    maven,
    scalatest,
    jdt,
    scalariform,
    asm,
    asmCommons
  )

  def copyTo(target: File)(src: File) = (src, if(target.isDirectory) target / src.name else target) 
  
  def copyFileToDirectory(src: File, target: File) = copyFilesToDirectory(List(src), target)

  def copyFilesToDirectory(srcs: Traversable[File], target: File) =
        if(target.isDirectory) copy( srcs map copyTo(target)) 
        else throw new IllegalArgumentException("Target should be a directory")
  
  import java.io.BufferedReader
  import java.io.BufferedWriter

  def using(in: File, out: File)(process: (BufferedReader, BufferedWriter) => Unit) = {
    import java.io.FileReader
    import java.io.FileWriter
    
    val input = new BufferedReader(new FileReader(in))
    val output= new BufferedWriter(new FileWriter(out))
    
    try {
      process(input, output)
      Right("Success")
    }
    catch {
      case e => Left(e)
    }
    finally {
      input.close()
      output.close()
    }
  }
  
  def copyWithLineProcessing(from: File, to: File, f: (String) => String) = {
    val target = if(to.isDirectory) to / from.name else to
    using(from, target) {
      (in, out) => 
        foreachLine(in) {
          (line) => 
            out.write(f(line))
            out.newLine()
        }
        to.setExecutable(true)
    }
  }

  private def doSh(str:String, cwd: File) = Process("sh" :: "-c" :: str :: Nil, cwd)
  private def doSh(str:String) = Process("sh" :: "-c" :: str :: Nil)

  val stage = TaskKey[(File)]("stage", "Build the deployment directory structure.")
  lazy val stageTask = stage <<=  (fullClasspath in Compile, streams, packageBin in Compile) map { 
    (cp:Classpath, s: TaskStreams, jar) =>
      import s.log._

      val dist: File = "dist"
      val distBin = dist / "bin"
      val distLib = dist / "lib"
      val distElisp = dist / "elisp"

      val src: File = "src"
      val srcScripts: File = "etc/scripts"
      val etc: File = "etc"
      
      val readme: File = "README.md"
      val license: File = "LICENSE"

      debug("Deleting the dist directory")
      delete(dist)

      debug("Creating dist directories")
      createDirectories(List(dist, distBin, distLib, distElisp))
    
      info("Copying elisp environment to: %s".format(distElisp))
      val elisp = src / "main" / "elisp" ** "*.el"
      copyFilesToDirectory(elisp.get, distElisp)

      info("Copying jars to: %s".format(distLib))
      val deps = jar +: cp.files.filter(f => !(f.isDirectory))
      //val jars = in.confg.classpath.toList.filter(f => !(f.isDirectory))
      copyFilesToDirectory(deps, distLib)

      // Grab all jars..
      val cpLibs = (distLib ** "*.jar").get
      val cpUnix = cpLibs.mkString(":").replace("\\", "/")
      val cpWindows = "\"" + cpLibs.mkString(";").replace("/", "\\") + "\""

      copyWithLineProcessing(srcScripts / "server", distBin, 
                             _.replace("<RUNTIME_CLASSPATH>", cpUnix)) match {
        case Left(e) => error("Failed to process the *nix script template.")
        case _       => info("*nix server script copied successfully")
      }

      copyWithLineProcessing(srcScripts / "server.bat", distBin, 
                             _.replace("<RUNTIME_CLASSPATH>", cpWindows)) match {
        case Left(e) => error("Failed to process the windows script template.")
        case _       => info("Windows server script copied successfully")
      }

      info("Copying readme and license to: %s".format(dist))
      copyFilesToDirectory(List(readme, license), dist)
      
      // Hack to make 2.8.1 ENSIME work for 2.8.0 projects
      copyFileToDirectory(etc / "implicitNotFound.jar", distLib)
      dist
  }

  logLevel in stage := Level.Debug

  val dist = TaskKey[Unit]("dist", "Compress the deployment directory")

  lazy val distTask = dist <<= (stage, artifact, packageBin in Compile, baseDirectory, streams) map {
    (dist, a, pkg, basedir, s) =>
      import s.log._

      val archiveFile = (basedir / (pkg.base + ".tar.gz")).getCanonicalPath
      println("artifact:" + a.name)
      withTemporaryDirectory { 
        tmp =>
          val tmpRelease = tmp / a.name
          info("Copying ./dist to temp directory: " + tmpRelease)
          doSh("cp -r ./dist " + tmpRelease) !! s.log
          info("Compressing temp directory to " + archiveFile + "...")
          println("tar -pcvzf " + archiveFile + " " + a.name)
          doSh("tar -pcvzf " + archiveFile + " " + a.name, tmp) !! s.log
      }
  }
  
  val publishManual = TaskKey[Unit]("publish-manual", "Publish the manual")

  lazy val publishManualTask = publishManual <<= (streams) map {
    (s) => 
      import s.log._
      
      info("Converting manual to HTML")
      withTemporaryDirectory {
        tmp =>
          val target = tmp / "ensime_manual.html"
          val etc: File = "etc"
          
          doSh("pdflatex manual.ltx", etc) !! s.log
          doSh("cat manual_head.html > " + target, etc) !! s.log
          doSh("tth -r -u -Lmanual < manual.ltx >> " + target, etc) !! s.log
          doSh("cat manual_tail.html >> " + target, etc) !! s.log
          info("Publishing manual to web...")
          doSh("scp " + target + " www@aemon.com:~/public/aemon/file_dump/", etc) !! s.log
          
      }
  }

  def tasks = Seq( stageTask, distTask, publishManualTask )

  lazy val ensime = Project (
    "ensime",
    file ("."),
    settings = buildSettings ++ Seq (libraryDependencies := commonDeps)   
  ) settings( tasks:_* )

}
 
