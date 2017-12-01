// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime

import shapeless._

import org.ensime.sexp._

package object api {
  private[this] val _swanky: SexpConfig[Nothing] =
    s => SexpSymbol(":ensime-api-" + SexpConfig.dashify(s))
  implicit def swanky[A, Repr <: Coproduct](
    implicit G: Generic.Aux[A, Repr]
  ): SexpConfig[A] = _swanky.asInstanceOf[SexpConfig[A]]
}
