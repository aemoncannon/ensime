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

package org.ensime.server

import java.io.File
import java.net.URLClassLoader
import scala.util.matching.Regex
import com.typesafe.zinc.{ ZincClient }
import xsbti.api.Compilation
import org.ensime.config.ProjectConfig

class ZincBuilder(config: ProjectConfig) {
  println(s"ZincBuilder(init): compileDeps = ${config.compileDeps}")
  println(s"ZincBuilder(init): compilerClasspath = ${config.compilerClasspath}")

  val classpathFiles = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs map (url => new File(url.toURI))
  private def locateJar(regex: Regex): File =
    classpathFiles
      .find(f => regex.findFirstMatchIn(f.toString).isDefined)
      .getOrElse(throw new Exception(s"Error locating $regex in classpath"))

  val cwd = config.root
  val compilerClasspath = config.compilerClasspath.split(File.pathSeparator) map (new File(_)) toSeq
  val scalaCompiler = config.scalaCompilerJar.getOrElse(locateJar("""(.*scala-compiler.*\.jar)""".r))
  val scalaLibrary = config.scalaLibraryJar.getOrElse(locateJar("""(.*scala-library.*\.jar)""".r))
  val scalaExtra = List(config.scalaReflectJar.getOrElse(locateJar("""(.*scala-reflect.*\.jar)""".r)))
  val sbtInterface = new File(classOf[Compilation].getProtectionDomain.getCodeSource.getLocation.toURI)
  val compilerInterface = locateJar("""(.*compiler-interface.*\.jar)""".r)

  val target = config.target.map(_.toString).getOrElse(".")
  val compilerArgs = List(
    "-compile-order", "scala-then-java",
//    "-S", config.extraCompilerArgs.mkString("\"", " ", "\""),
    "-scala-compiler", scalaCompiler.toString,
    "-scala-library", scalaLibrary.toString,
    "-scala-extra", scalaExtra.mkString(","),
    "-sbt-interface", sbtInterface.toString,
    "-compiler-interface", compilerInterface.toString,
    "-classpath", compilerClasspath.mkString(":"),
    "-d", target)

  val zincClient =
    try {
      val zincLibDir = new File(config.zincLibDir.getOrElse(findDefaultZincDir))
      val zincClasspath = zincLibDir.listFiles().filter(_.getName.endsWith(".jar"))
      val z = new ZincClient()
      if (z.requireServer(Seq.empty[String], zincClasspath, "30000")) Some(z)
      else None
    } catch {
      case t: Throwable =>
        val sw = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(sw, true))
        println(s"Error:  Error creating Zinc server.  $t\n$sw")
        None
    }

  println(s"ZincBuilder(init): zincClient=$zincClient")

  def compile(sourceFiles: List[File]) = {
    println("compile:  IN")
    try {
      val sources = sourceFiles map (_.getPath)
      val args = compilerArgs ++ sources
      println(s"args=${args.mkString(" ")}")
      zincClient match {
        case Some(z) => z.run(compilerArgs ++ sources, cwd, Console.out, Console.err)
        case None => 1 // Report error to front end
      }
    } catch {
      case t: Throwable =>
        val sw = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(sw, true))
        println(s"Error:  Error running compile.  $t\n$sw")
    }
    println("compile:  OUT")
  }

  private def findDefaultZincDir(): String = {
    val ensimeLibDir = locateJar("""ensime_.*\.jar""".r)
    new File(new File(ensimeLibDir.getParentFile.getParentFile.getParentFile, "2.10"), "lib").getPath
  }
}
