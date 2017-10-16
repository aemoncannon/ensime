// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/**
 * Provides the JsonFormats for the most important Scala types.
 */
trait BasicFormats {

  // prefer java.math.BigDecimal to scala.BigDecimal
  implicit val bigDecimal: JsonFormat[BigDecimal] =
    JsonFormat.instance[BigDecimal](JsNumber(_)) {
      case JsNumber(x) => x
      case JsString(x) => BigDecimal(x)
      case x           => deserializationError("Expected JsNumber, got " + x)
    }

  implicit val int: JsonFormat[Int] =
    bigDecimal.xmap(_.intValue, BigDecimal(_))

  implicit val long: JsonFormat[Long] =
    bigDecimal.xmap(_.longValue, BigDecimal(_))

  implicit val float: JsonFormat[Float] =
    bigDecimal.xmap(_.floatValue, f => BigDecimal(f.toDouble))

  implicit val double: JsonFormat[Double] =
    bigDecimal.xmap(_.doubleValue, BigDecimal(_))

  implicit val byte: JsonFormat[Byte] =
    bigDecimal.xmap(_.byteValue, b => BigDecimal(b.toInt))

  implicit val short: JsonFormat[Short] =
    bigDecimal.xmap(_.shortValue, s => BigDecimal(s.toInt))

  // new java.math.BigDecimal("1e2147483647").toBigInteger
  // hangs forever... and we are susceptible.
  implicit val bigInt: JsonFormat[BigInt] =
    bigDecimal.xmap(_.toBigInt, BigDecimal(_))

  implicit val unit: JsonFormat[Unit] =
    JsonFormat.instance[Unit](_ => JsNumber(1))(_ => ())

  implicit val boolean: JsonFormat[Boolean] =
    JsonFormat.instance[Boolean](JsBoolean(_)) {
      case JsTrue  => true
      case JsFalse => false
      case x       => deserializationError("Expected JsBoolean, but got " + x)
    }

  implicit val string: JsonFormat[String] =
    JsonFormat.instance[String](JsString(_)) {
      case JsString(x) => x
      case x =>
        deserializationError("Expected String as JsString, but got " + x)
    }

  implicit val char: JsonFormat[Char] = string.xmap({ x =>
    if (x.length == 1) x.charAt(0)
    else deserializationError("Expected single-character, got " + x)
  }, c => String.valueOf(c))

  implicit val symbol: JsonFormat[Symbol] =
    JsonFormat.instance[Symbol](s => JsString(s.name)) {
      case JsString(x) => Symbol(x)
      case x =>
        deserializationError("Expected Symbol as JsString, but got " + x)
    }
}
