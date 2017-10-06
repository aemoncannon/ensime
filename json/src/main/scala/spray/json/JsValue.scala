// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/**
 * The general type of a JSON AST node.
 */
sealed abstract class JsValue {
  override def toString                      = compactPrint
  def toString(printer: (JsValue => String)) = printer(this)
  def compactPrint                           = CompactPrinter(this)
  def prettyPrint                            = PrettyPrinter(this)
  def convertTo[T: JsonReader]: T            = JsonReader[T].read(this)
}

/**
 * A JSON object.
 */
final case class JsObject(fields: Map[String, JsValue]) extends JsValue
object JsObject {
  val empty                    = JsObject(Map.empty[String, JsValue])
  def apply(members: JsField*) = new JsObject(Map(members: _*))
}

/**
 * A JSON array.
 */
final case class JsArray(elements: Vector[JsValue]) extends JsValue
object JsArray {
  val empty                     = JsArray(Vector.empty)
  def apply(elements: JsValue*) = new JsArray(elements.toVector)
}

/**
 * A JSON string.
 */
final case class JsString(value: String) extends JsValue

object JsString {
  val empty                = JsString("")
  def apply(value: Symbol) = new JsString(value.name)
}

/**
 * A JSON number.
 */
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

/**
 * JSON Booleans.
 */
sealed abstract class JsBoolean extends JsValue {
  def value: Boolean
}
object JsBoolean {
  def apply(x: Boolean): JsBoolean           = if (x) JsTrue else JsFalse
  def unapply(x: JsBoolean): Option[Boolean] = Some(x.value)
}
case object JsTrue extends JsBoolean {
  def value = true
}
case object JsFalse extends JsBoolean {
  def value = false
}

/**
 * The representation for JSON null.
 */
case object JsNull extends JsValue
