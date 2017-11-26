// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import scala.collection.immutable.{ ListMap, Seq }

/** A JSON AST node. */
sealed abstract class JsValue

final case class JsObject(fields: Map[String, JsValue]) extends JsValue
object JsObject {
  def apply(members: (String, JsValue)*): JsObject = JsObject(Map(members: _*))
  val empty: JsObject                              = JsObject(ListMap.empty[String, JsValue])
}

final case class JsArray(elements: Seq[JsValue]) extends JsValue
object JsArray {
  def apply(members: JsValue*): JsArray = JsArray(List(members: _*))
  val empty: JsArray                    = JsArray(List.empty)
}

case object JsNull extends JsValue

final case class JsBoolean(value: Boolean) extends JsValue
object JsBoolean {
  val True: JsBoolean  = JsBoolean(true)
  val False: JsBoolean = JsBoolean(false)
}

final case class JsString(value: String) extends JsValue
object JsString {
  val empty                = JsString("")
  def apply(value: Symbol) = new JsString(value.name)
}

final case class JsNumber(value: BigDecimal) extends JsValue
object JsNumber {
  val zero: JsNumber = apply(0)
  def apply(n: Int)  = new JsNumber(BigDecimal(n))
  def apply(n: Long) = new JsNumber(BigDecimal(n))
  def apply(n: Double) = n match {
    case n if n.isNaN      => JsNull
    case n if n.isInfinity => JsNull
    case _                 => new JsNumber(BigDecimal(n))
  }
  def apply(n: BigInt)      = new JsNumber(BigDecimal(n))
  def apply(n: String)      = new JsNumber(BigDecimal(n))
  def apply(n: Array[Char]) = new JsNumber(BigDecimal(n))
}
