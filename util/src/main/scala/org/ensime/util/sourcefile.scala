// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import org.ensime.api._
import org.ensime.util.ensimefile._

/**
 * Functionality for the SourceFileInfo family.
 */
package object sourcefile {

  implicit class RichSourceFileInfo(private val v: SourceFileInfo)
      extends AnyVal {
    def exists() = v match {
      case SourceFileInfo(f, _, _, _) if f.exists()       => true
      case SourceFileInfo(_, Some(c), _, _)               => true
      case SourceFileInfo(_, _, Some(f), _) if f.exists() => true
      case _                                              => false
    }
  }

}
