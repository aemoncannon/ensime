// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

package object option {
  implicit class RichOption[T](o: Option[T]) {
    def getOrThrow(msg: String): T =
      o.getOrElse(throw new IllegalArgumentException(msg))
  }
}
