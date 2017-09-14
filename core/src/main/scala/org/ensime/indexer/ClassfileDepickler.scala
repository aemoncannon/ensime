// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.nio.file.NoSuchFileException
import scala.tools.scalap.scalax.rules.scalasig._

import org.ensime.core.ScalapSymbolToFqn
import org.ensime.util.io._

import java.nio.file.{ Path, Files, StandardOpenOption }

class ClassfileDepickler(path: Path) extends ScalapSymbolToFqn {

  val scalasig: Option[ScalaSig] = depickle

  /** Uses scalap to produce a scala reflective view of the classfile */
  private def depickle: Option[ScalaSig] = {
    try {
      val bytes = Files.readAllBytes(path)
      val byteCode = ByteCode(bytes)
      val classFile = ClassFileParser.parse(byteCode)
      ScalaSigParser.parse(classFile)
    } catch {
      // ClassFileParser fails to parse some JDK class files
      case e: Exception => None
    }
  }

  private val ignore = Set("<local child>", "<refinement>", "anon")
  def getClasses: Map[String, RawScalapClass] = scalasig.fold(Map.empty[String, RawScalapClass]) { sig =>
    sig.symbols.collect {
      case s: ClassSymbol if !(ignore.exists(s.name.contains) || s.isSynthetic) =>
        val aClass = rawScalaClass(s)
        aClass.javaName.fqnString -> aClass
    }.toMap
  }
}
