// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp.examples

import scalaz.deriving

import org.ensime.sexp._
import scalaz.xderiving

@xderiving(SexpWriter, SexpReader) final case class Optimal(thing: String)
    extends AnyVal

@deriving(SexpWriter, SexpReader) sealed trait SimpleTrait
@deriving(SexpWriter, SexpReader) final case class Foo(s: String)
    extends SimpleTrait
@deriving(SexpWriter, SexpReader) final case class Bar() extends SimpleTrait
@deriving(SexpWriter, SexpReader) case object Baz        extends SimpleTrait
@deriving(SexpWriter, SexpReader) final case class Faz(o: Option[String])
    extends SimpleTrait

@deriving(SexpWriter, SexpReader) final case class Recursive(
  h: String,
  t: Option[Recursive] = None
)

@deriving(SexpWriter, SexpReader) sealed abstract class AbstractThing(
  val id: String
)
object AbstractThing {
  implicit val sexpConfig: SexpConfig[AbstractThing] =
    s => SexpSymbol(":" + s.toUpperCase)

}
@deriving(SexpWriter, SexpReader) case object Wibble
    extends AbstractThing("wibble")
@deriving(SexpWriter, SexpReader) final case class Wobble(
  override val id: String
) extends AbstractThing(id)
object Wobble {
  implicit val sexpConfig: SexpConfig[Wobble] =
    s => SexpSymbol(":" + s.toUpperCase)

}
