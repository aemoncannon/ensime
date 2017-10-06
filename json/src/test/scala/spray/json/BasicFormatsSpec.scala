// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import org.scalatest._
import Matchers._

class BasicFormatsSpec extends WordSpec with DefaultJsonProtocol {

  "The IntJsonFormat" should {
    "convert an Int to a JsNumber" in {
      42.toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to an Int" in {
      JsNumber(42).convertTo[Int] shouldEqual 42
    }
  }

  "The LongJsonFormat" should {
    "convert a Long to a JsNumber" in {
      7563661897011259335L.toJson shouldEqual JsNumber(7563661897011259335L)
    }
    "convert a JsNumber to a Long" in {
      JsNumber(7563661897011259335L)
        .convertTo[Long] shouldEqual 7563661897011259335L
    }
  }

  "The FloatJsonFormat" should {
    "convert a Float to a JsNumber" in {
      4.2f.toJson shouldEqual JsNumber(4.2f)
    }
    "convert a Float.NaN to a JsNull" in {
      Float.NaN.toJson shouldEqual JsNull
    }
    "convert a Float.PositiveInfinity to a JsNull" in {
      Float.PositiveInfinity.toJson shouldEqual JsNull
    }
    "convert a Float.NegativeInfinity to a JsNull" in {
      Float.NegativeInfinity.toJson shouldEqual JsNull
    }
    "convert a JsNumber to a Float" in {
      JsNumber(4.2f).convertTo[Float] shouldEqual 4.2f
    }
    "convert a JsNull to a Float" in {
      JsNull.convertTo[Float].isNaN shouldEqual Float.NaN.isNaN
    }
  }

  "The DoubleJsonFormat" should {
    "convert a Double to a JsNumber" in {
      4.2.toJson shouldEqual JsNumber(4.2)
    }
    "convert a Double.NaN to a JsNull" in {
      Double.NaN.toJson shouldEqual JsNull
    }
    "convert a Double.PositiveInfinity to a JsNull" in {
      Double.PositiveInfinity.toJson shouldEqual JsNull
    }
    "convert a Double.NegativeInfinity to a JsNull" in {
      Double.NegativeInfinity.toJson shouldEqual JsNull
    }
    "convert a JsNumber to a Double" in {
      JsNumber(4.2).convertTo[Double] shouldEqual 4.2
    }
    "convert a JsNull to a Double" in {
      JsNull.convertTo[Double].isNaN shouldEqual Double.NaN.isNaN
    }
  }

  "The ByteJsonFormat" should {
    "convert a Byte to a JsNumber" in {
      42.asInstanceOf[Byte].toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a Byte" in {
      JsNumber(42).convertTo[Byte] shouldEqual 42
    }
  }

  "The ShortJsonFormat" should {
    "convert a Short to a JsNumber" in {
      42.asInstanceOf[Short].toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a Short" in {
      JsNumber(42).convertTo[Short] shouldEqual 42
    }
  }

  "The BigDecimalJsonFormat" should {
    "convert a BigDecimal to a JsNumber" in {
      BigDecimal(42).toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a BigDecimal" in {
      JsNumber(42).convertTo[BigDecimal] shouldEqual BigDecimal(42)
    }
    """convert a JsString to a BigDecimal (to allow the quoted-large-numbers pattern)""" in {
      JsString("9223372036854775809")
        .convertTo[BigDecimal] shouldEqual BigDecimal("9223372036854775809")
    }
  }

  "The BigIntJsonFormat" should {
    "convert a BigInt to a JsNumber" in {
      BigInt(42).toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a BigInt" in {
      JsNumber(42).convertTo[BigInt] shouldEqual BigInt(42)
    }
    """convert a JsString to a BigInt (to allow the quoted-large-numbers pattern)""" in {
      JsString("9223372036854775809").convertTo[BigInt] shouldEqual BigInt(
        "9223372036854775809"
      )
    }
  }

  "The UnitJsonFormat" should {
    "convert Unit to a JsNumber(1)" in {
      ().toJson shouldEqual JsNumber(1)
    }
    "convert a JsNumber to Unit" in {
      JsNumber(1).convertTo[Unit] shouldEqual (())
    }
  }

  "The BooleanJsonFormat" should {
    "convert true to a JsTrue" in { true.toJson shouldEqual JsTrue }
    "convert false to a JsFalse" in { false.toJson shouldEqual JsFalse }
    "convert a JsTrue to true" in { JsTrue.convertTo[Boolean] shouldEqual true }
    "convert a JsFalse to false" in {
      JsFalse.convertTo[Boolean] shouldEqual false
    }
  }

  "The CharJsonFormat" should {
    "convert a Char to a JsString" in {
      'c'.toJson shouldEqual JsString("c")
    }
    "convert a JsString to a Char" in {
      JsString("c").convertTo[Char] shouldEqual 'c'
    }
  }

  "The StringJsonFormat" should {
    "convert a String to a JsString" in {
      "Hello".toJson shouldEqual JsString("Hello")
    }
    "convert a JsString to a String" in {
      JsString("Hello").convertTo[String] shouldEqual "Hello"
    }
  }

  "The SymbolJsonFormat" should {
    "convert a Symbol to a JsString" in {
      'Hello.toJson shouldEqual JsString("Hello")
    }
    "convert a JsString to a Symbol" in {
      JsString("Hello").convertTo[Symbol] shouldEqual 'Hello
    }
  }

}
