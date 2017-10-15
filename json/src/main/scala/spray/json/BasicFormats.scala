// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/**
 * Provides the JsonFormats for the most important Scala types.
 */
trait BasicFormats {

  implicit val int: JsonFormat[Int] = JsonFormat.instance[Int](JsNumber(_)) {
    case JsNumber(x) => x.intValue
    case x           => deserializationError("Expected Int as JsNumber, but got " + x)
  }

  implicit val long: JsonFormat[Long] = JsonFormat.instance[Long](JsNumber(_)) {
    case JsNumber(x) => x.longValue
    case x           => deserializationError("Expected Long as JsNumber, but got " + x)
  }

  implicit val float: JsonFormat[Float] =
    JsonFormat.instance[Float](f => JsNumber(f.toDouble)) {
      case JsNumber(x) => x.floatValue
      case JsNull      => Float.NaN
      case x           => deserializationError("Expected Float as JsNumber, but got " + x)
    }

  implicit val double: JsonFormat[Double] =
    JsonFormat.instance[Double](JsNumber(_)) {
      case JsNumber(x) => x.doubleValue
      case JsNull      => Double.NaN
      case x =>
        deserializationError("Expected Double as JsNumber, but got " + x)
    }

  implicit val byte: JsonFormat[Byte] =
    JsonFormat.instance[Byte](b => JsNumber(b.toInt)) {
      case JsNumber(x) => x.byteValue
      case x           => deserializationError("Expected Byte as JsNumber, but got " + x)
    }

  implicit val short: JsonFormat[Short] =
    JsonFormat.instance[Short](s => JsNumber(s.toInt)) {
      case JsNumber(x) => x.shortValue
      case x           => deserializationError("Expected Short as JsNumber, but got " + x)
    }

  implicit val bigDecimal: JsonFormat[BigDecimal] =
    JsonFormat.instance[BigDecimal](JsNumber(_)) {
      case JsNumber(x) => x
      case JsString(x) => BigDecimal(x)
      case x =>
        deserializationError("Expected BigDecimal as JsNumber, but got " + x)
    }

  implicit val bigInt: JsonFormat[BigInt] =
    JsonFormat.instance[BigInt](JsNumber(_)) {
      case JsNumber(x) => x.toBigInt
      case JsString(x) => BigInt(x)
      case x =>
        deserializationError("Expected BigInt as JsNumber, but got " + x)
    }

  implicit val unit: JsonFormat[Unit] =
    JsonFormat.instance[Unit](_ => JsNumber(1))(_ => ())

  implicit val boolean: JsonFormat[Boolean] =
    JsonFormat.instance[Boolean](JsBoolean(_)) {
      case JsTrue  => true
      case JsFalse => false
      case x       => deserializationError("Expected JsBoolean, but got " + x)
    }

  implicit val char: JsonFormat[Char] =
    JsonFormat.instance[Char](c => JsString(String.valueOf(c))) {
      case JsString(x) if x.length == 1 => x.charAt(0)
      case x =>
        deserializationError(
          "Expected Char as single-character JsString, but got " + x
        )
    }

  implicit val string: JsonFormat[String] =
    JsonFormat.instance[String](JsString(_)) {
      case JsString(x) => x
      case x =>
        deserializationError("Expected String as JsString, but got " + x)
    }

  implicit val symbol: JsonFormat[Symbol] =
    JsonFormat.instance[Symbol](s => JsString(s.name)) {
      case JsString(x) => Symbol(x)
      case x =>
        deserializationError("Expected Symbol as JsString, but got " + x)
    }
}
