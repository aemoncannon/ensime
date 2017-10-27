// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File

import scala.util.Try
import org.ensime.api._
import org.ensime.util.LegacyArchiveExtraction
import org.ensime.util.ensimefile._
import org.ensime.util.file._
import shapeless._

/**
 * Goes through sealed families and gets the canonical path of `File`
 * and `Path` instances.
 *
 * Not to be confused with "the other" Cannon ;-)
 */
object Canon extends Poly1 {
  // people extend File, so we have to handle subtypes
  implicit def caseFile[F <: File]: Case[F] { type Result = File } =
    at[F](f => f.canon)

  var serverConfig: EnsimeServerConfig = null

  var config: EnsimeConfig = null // yes, I know...
  // we really want extractor to be a constructor parameter to a Canon instance

  private def extractor: Option[LegacyArchiveExtraction] =
    if ((serverConfig != null && !serverConfig.legacy.jarurls) || config == null)
      None
    else Some(new LegacyArchiveExtraction(config.cacheDir.file))
  implicit def caseEnsimeFile[EF <: EnsimeFile]
    : Case[EF] { type Result = EnsimeFile } = at[EF] { f =>
    extractor.flatMap { extractor =>
      Try(extractor.write(f)).toOption
    }.getOrElse(f).canon
  }
}

object Canonised {
  def apply[T](t: T)(implicit everywhere: Everywhere[Canon.type, T]) =
    everywhere(t)
}
