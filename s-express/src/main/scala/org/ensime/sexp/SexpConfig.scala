// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import simulacrum._

@typeclass trait SexpConfig[A] {
  def hint(s: String): SexpSymbol
}
object SexpConfig {
  private[this] val _default: SexpConfig[Nothing] = s =>
    SexpSymbol(":" + dashify(s))
  implicit def default[A]: SexpConfig[A] = _default.asInstanceOf[SexpConfig[A]]

  def dashify(field: String): String =
    field.replaceAll("([A-Z])", "-$1").toLowerCase.replaceAll("^-", "")

}
