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
import com.typesafe.zinc.{ Settings, Setup, Inputs, Parsed, Compiler, Util }
import xsbti.{ F0, Logger }
import xsbti.api.Compilation
import org.ensime.config.ProjectConfig

class ZincBuilder(config: ProjectConfig) {
  import Util._

  println(s"ZindBuilder(init): compileDeps = ${config.compileDeps}")
  println(s"ZindBuilder(init): compilerClasspath = ${config.compilerClasspath}")
  val Parsed(rawSettings, residual, errors) = Settings.parse(config.builderArgs)
  val settings = Settings.normalise(rawSettings, Some(config.root))
  val baseInputs = Inputs(settings)
  val logger = new Logger {
    def error(arg: F0[String]): Unit = println(s"Error: ${arg()}")
    def warn(arg: F0[String]): Unit = println(s"Warn: ${arg()}")
    def info(arg: F0[String]): Unit = println(s"Info: ${arg()}")
    def debug(arg: F0[String]): Unit = println(s"Debug: ${arg()}")
    def trace(arg: F0[Throwable]): Unit = println(s"Trace: ${arg()}")
  }

  val classpathURLs = Thread.currentThread.getContextClassLoader.asInstanceOf[URLClassLoader].getURLs
  def locateJar(regex: Regex): File =
    classpathURLs
      .find(url => regex.findFirstMatchIn(url.toString).isDefined)
      .map(url => new File(url.toURI))
      .getOrElse(throw new Exception(s"Error locating $regex in classpath"))

  val compiler = try {

    val setup =
      Setup.setup(
        scalaCompiler = config.scalaCompilerJar.getOrElse(locateJar("""(.*scala-compiler.*\.jar)""".r)),
        scalaLibrary = config.scalaLibraryJar.getOrElse(locateJar("""(.*scala-library.*\.jar)""".r)),
        scalaExtra = List(config.scalaReflectJar.getOrElse(locateJar("""(.*scala-reflect.*\.jar)""".r))),
        sbtInterface = new File(classOf[Compilation].getProtectionDomain.getCodeSource.getLocation.toURI),
        compilerInterfaceSrc = locateJar("""(.*compiler-interface.*\.jar)""".r),
        javaHomeDir = None,
        forkJava = false)

    Some(Compiler(setup, logger))
  } catch {
    case t: Throwable =>
      println("Error creating Zinc compiler.  " + t.getStackTrace.mkString("\n"))
      None
  }

  def compile(sources: List[File]) = {
    println("compile:  IN")
    try {
      val inputs = baseInputs.copy(sources = sources)
      compiler foreach { _.compile(inputs, Some(config.root))(logger) }
    } catch {
      case t: Throwable =>
        val sw = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(sw, true))
        println(s"Error:  Error running compile.  $t\n$sw")
    }
    println("compile:  OUT")
  }
}
