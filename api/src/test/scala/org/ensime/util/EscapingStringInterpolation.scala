// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.util

import java.io.File
import org.ensime.api.RawFile

object EscapingStringInterpolation {

  /**
   * String interpolation that automatically escapes known "bad" types (such as
   * `File` on Windows) and *ONLY* for use in ENSIME tests when asserting on
   * wire formats.
   */
  final case class StringContext(parts: String*) {
    private val delegate = new scala.StringContext(parts: _*)
    def s(args: Any*): String = {
      val hijacked = args.map {
        case f: File       => f.toString.replace("""\""", """\\""")
        case RawFile(path) => path.toFile.toString.replace("""\""", """\\""")
        case other         => other
      }
      delegate.s(hijacked: _*)
    }
  }
}
