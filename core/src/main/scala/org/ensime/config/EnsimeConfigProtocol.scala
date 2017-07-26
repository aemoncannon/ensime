// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import java.nio.file.{ Files, Path, Paths }

import akka.event.slf4j.Logger
import shapeless._

import org.ensime.sexp._
import org.ensime.sexp.formats._
import org.ensime.core.Canonised

import org.ensime.util.path._

import org.ensime.api._

import scala.collection.JavaConverters._

object EnsimeConfigProtocol {
  object Protocol extends DefaultSexpProtocol
    with OptionAltFormat
    with CamelCaseToDashes
  import org.ensime.config.EnsimeConfigProtocol.Protocol._

  private def log = Logger(this.getClass.getName)

  implicit object EnsimeFileFormat extends SexpFormat[RawFile] {
    def write(f: RawFile): Sexp = SexpString(f.path.toString)
    def read(sexp: Sexp): RawFile = sexp match {
      case SexpString(file) => RawFile(Paths.get(file))
      case got => deserializationError(got)
    }
  }

  private implicit val projectIdFormat: SexpFormat[EnsimeProjectId] = cachedImplicit
  private implicit val projectFormat: SexpFormat[EnsimeProject] = cachedImplicit
  private implicit val configFormat: SexpFormat[EnsimeConfig] = cachedImplicit

  def parse(config: String): EnsimeConfig = {
    val raw = config.parseSexp.convertTo[EnsimeConfig]
    validated(raw)
  }

  def validated(c: EnsimeConfig): EnsimeConfig = {
    // cats.data.Validated would be a cleaner way to do this
    {
      import c._
      val files = (rootDir :: javaHome :: javaSources).map(_.path)
      (files ::: javaRunTime(c)).foreach { f =>
        require(f.exists, "" + f + " is required but does not exist")
      }
    }

    c.copy(
      projects = c.projects.map(validated)
    )
  }

  def javaRunTime(c: EnsimeConfig): List[Path] =
    Files.walk(c.javaHome.path)
      .iterator
      .asScala
      .filter(_.getFileName.toString == "rt.jar")
      .toList
  /*
   We use the canonical form of files/directories to keep OS X happy
   when loading S-Expressions. But the canon may fail to resolve if
   the file/directory does not exist, so we force create all required
   directories and then re-canon them, which is - admittedly - a weird
   side-effect.
   */
  private[config] def validated(p: EnsimeProject): EnsimeProject = {
    (p.targets ++ p.sources).foreach { dir =>
      if (!dir.exists && !dir.isJar) {
        log.warn(s"$dir does not exist, creating")
        dir.path.mkdirs()
      }
    }
    Canonised(p)
  }
}
