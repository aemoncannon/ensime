// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import org.scalatest._
import Matchers._

import JsReader.ops._
import JsWriter.ops._

class BasicFormatsSpec extends WordSpec {

  "The Int formats" should {
    "convert an Int to a JsNumber" in {
      42.toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to an Int" in {
      JsNumber(42).as[Int] shouldEqual Right(42)
    }
  }

  "The Long formats" should {
    "convert a Long to a JsNumber" in {
      7563661897011259335L.toJson shouldEqual JsNumber(7563661897011259335L)
    }
    "convert a JsNumber to a Long" in {
      JsNumber(7563661897011259335L)
        .as[Long] shouldEqual Right(7563661897011259335L)
    }
  }

  "The Float formats" should {
    "convert a Float to a JsNumber" in {
      4.2f.toJson shouldEqual JsNumber(4.2f)
    }
    "convert a JsNumber to a Float" in {
      JsNumber(4.2f).as[Float] shouldEqual Right(4.2f)
    }
  }

  "The Double formats" should {
    "convert a Double to a JsNumber" in {
      4.2.toJson shouldEqual JsNumber(4.2)
    }
    "convert a JsNumber to a Double" in {
      JsNumber(4.2).as[Double] shouldEqual Right(4.2)
    }
  }

  "The Byte formats" should {
    "convert a Byte to a JsNumber" in {
      42.asInstanceOf[Byte].toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a Byte" in {
      JsNumber(42).as[Byte] shouldEqual Right(42)
    }
  }

  "The Short formats" should {
    "convert a Short to a JsNumber" in {
      42.asInstanceOf[Short].toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a Short" in {
      JsNumber(42).as[Short] shouldEqual Right(42)
    }
  }

  "The BigDecimal formats" should {
    "convert a BigDecimal to a JsNumber" in {
      BigDecimal(42).toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a BigDecimal" in {
      JsNumber(42).as[BigDecimal] shouldEqual Right(BigDecimal(42))
    }
    """convert a JsString to a BigDecimal (to allow the quoted-large-numbers pattern)""" in {
      JsString("9223372036854775809")
        .as[BigDecimal] shouldEqual Right(BigDecimal("9223372036854775809"))
    }
  }

  "The BigInt formats" should {
    "convert a BigInt to a JsNumber" in {
      BigInt(42).toJson shouldEqual JsNumber(42)
    }
    "convert a JsNumber to a BigInt" in {
      JsNumber(42).as[BigInt] shouldEqual Right(BigInt(42))
    }
    """convert a JsString to a BigInt (to allow the quoted-large-numbers pattern)""" in {
      JsString("9223372036854775809").as[BigInt] shouldEqual Right(
        BigInt(
          "9223372036854775809"
        )
      )
    }
  }

  "The Unit formats" should {
    "convert Unit to a JsNumber(1)" in {
      ().toJson shouldEqual JsNumber(1)
    }
    "convert a JsNumber to Unit" in {
      JsNumber(1).as[Unit] shouldEqual Right(())
    }
  }

  "The Boolean formats" should {
    "convert true to a JsBoolean.True" in {
      true.toJson shouldEqual JsBoolean.True
    }
    "convert false to a JsBoolean.False" in {
      false.toJson shouldEqual JsBoolean.False
    }
    "convert a JsBoolean.True to true" in {
      JsBoolean.True.as[Boolean] shouldEqual Right(true)
    }
    "convert a JsBoolean.False to false" in {
      JsBoolean.False.as[Boolean] shouldEqual Right(false)
    }
  }

  "The Char formats" should {
    "convert a Char to a JsString" in {
      'c'.toJson shouldEqual JsString("c")
    }
    "convert a JsString to a Char" in {
      JsString("c").as[Char] shouldEqual Right('c')
    }
  }

  "The String formats" should {
    "convert a String to a JsString" in {
      "Hello".toJson shouldEqual JsString("Hello")
    }
    "convert a JsString to a String" in {
      JsString("Hello").as[String] shouldEqual Right("Hello")
    }
  }

  "The Symbol formats" should {
    "convert a Symbol to a JsString" in {
      'Hello.toJson shouldEqual JsString("Hello")
    }
    "convert a JsString to a Symbol" in {
      JsString("Hello").as[Symbol] shouldEqual Right('Hello)
    }
  }

}
