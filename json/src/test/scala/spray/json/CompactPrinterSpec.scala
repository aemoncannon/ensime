// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import org.scalatest._
import Matchers._

class CompactPrinterSpec extends WordSpec {

  "The CompactPrinter" should {
    "print JsNull to 'null'" in {
      CompactPrinter(JsNull) shouldEqual "null"
    }
    "print JsBoolean.True to 'true'" in {
      CompactPrinter(JsBoolean.True) shouldEqual "true"
    }
    "print JsBoolean.False to 'false'" in {
      CompactPrinter(JsBoolean.False) shouldEqual "false"
    }
    "print JsNumber(0) to '0'" in {
      CompactPrinter(JsNumber(0)) shouldEqual "0"
    }
    "print JsNumber(1.23) to '1.23'" in {
      CompactPrinter(JsNumber(1.23)) shouldEqual "1.23"
    }
    "print JsNumber(-1E10) to '-1E10'" in {
      CompactPrinter(JsNumber(-1E10)) shouldEqual "-1.0E+10"
    }
    "print JsNumber(12.34e-10) to '12.34e-10'" in {
      CompactPrinter(JsNumber(12.34e-10)) shouldEqual "1.234E-9"
    }
    "print JsString(\"xyz\") to \"xyz\"" in {
      CompactPrinter(JsString("xyz")) shouldEqual "\"xyz\""
    }
    "properly escape special chars in JsString" in {
      CompactPrinter(JsString("\"\\\b\f\n\r\t")) shouldEqual """"\"\\\b\f\n\r\t""""
      CompactPrinter(JsString("\u1000")) shouldEqual "\"\u1000\""
      CompactPrinter(JsString("\u0100")) shouldEqual "\"\u0100\""
      CompactPrinter(JsString("\u0010")) shouldEqual "\"\\u0010\""
      CompactPrinter(JsString("\u0001")) shouldEqual "\"\\u0001\""
      CompactPrinter(JsString("\u001e")) shouldEqual "\"\\u001e\""
      // don't escape as it isn't required by the spec
      CompactPrinter(JsString("\u007f")) shouldEqual "\"\u007f\""
      CompactPrinter(JsString("飞机因此受到损伤")) shouldEqual "\"飞机因此受到损伤\""
      CompactPrinter(JsString("\uD834\uDD1E")) shouldEqual "\"\uD834\uDD1E\""
    }
    "properly print a simple JsObject" in (
      CompactPrinter(
        JsObject("key" -> JsNumber(42), "key2" -> JsString("value"))
      )
        shouldEqual """{"key":42,"key2":"value"}"""
    )
    "properly print a simple JsArray" in (
      CompactPrinter(
        JsArray(JsNull, JsNumber(1.23), JsObject("key" -> JsBoolean(true)))
      )
        shouldEqual """[null,1.23,{"key":true}]"""
    )
    "properly print a JSON padding (JSONP) if requested" in {
      CompactPrinter(JsBoolean.True, Some("customCallback")) shouldEqual ("customCallback(true)")
    }
  }

}
