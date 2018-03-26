// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json.examples

import scalaz.deriving

import spray.json._
import scalaz.xderiving

@xderiving(JsWriter, JsReader) final case class Optimal(thing: String)
    extends AnyVal

@deriving(JsWriter, JsReader) sealed trait SimpleTrait
@deriving(JsWriter, JsReader) final case class Foo(s: String)
    extends SimpleTrait
@deriving(JsWriter, JsReader) final case class Bar() extends SimpleTrait
@deriving(JsWriter, JsReader) case object Baz        extends SimpleTrait
@deriving(JsWriter, JsReader) final case class Faz(o: Option[String])
    extends SimpleTrait

@deriving(JsWriter, JsReader) final case class Recursive(
  h: String,
  t: Option[Recursive] = None
)

@deriving(JsWriter, JsReader) sealed abstract class AbstractThing(
  val id: String
)
object AbstractThing {
  implicit val jsConfig: JsConfig[AbstractThing] =
    JsConfig[AbstractThing]("t", "v")
}
@deriving(JsWriter, JsReader) case object Wibble extends AbstractThing("wibble")
@deriving(JsWriter, JsReader) final case class Wobble(override val id: String)
    extends AbstractThing(id)

@deriving(JsWriter, JsReader)
sealed abstract class NotAnObject
final case class Time(s: String) extends NotAnObject
object Time {
  implicit val jsWriter: JsWriter[Time] = JsWriter[String].contramap(_.s)
  implicit val jsReader: JsReader[Time] = JsReader[String].map(Time(_))
}
@deriving(JsWriter, JsReader)
final case class Money(i: Int) extends NotAnObject
