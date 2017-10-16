// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import org.scalatest._
import Matchers._

class StandardFormatsSpec extends WordSpec with DefaultJsonProtocol {

  "The optionFormat" should {
    "convert None to JsNull" in {
      None.asInstanceOf[Option[Int]].toJson shouldEqual JsNull
    }
    "convert JsNull to None" in {
      JsNull.convertTo[Option[Int]] shouldEqual None
    }
    "convert Some(Hello) to JsString(Hello)" in {
      Some("Hello").asInstanceOf[Option[String]].toJson shouldEqual JsString(
        "Hello"
      )
    }
    "convert JsString(Hello) to Some(Hello)" in {
      JsString("Hello").convertTo[Option[String]] shouldEqual Some("Hello")
    }
  }

  "The eitherFormat" should {
    val a: Either[Int, String] = Left(42)
    val b: Either[Int, String] = Right("Hello")

    "convert the left side of an Either value to Json" in {
      a.toJson shouldEqual JsNumber(42)
    }
    "convert the right side of an Either value to Json" in {
      b.toJson shouldEqual JsString("Hello")
    }
    "convert the left side of an Either value from Json" in {
      JsNumber(42).convertTo[Either[Int, String]] shouldEqual Left(42)
    }
    "convert the right side of an Either value from Json" in {
      JsString("Hello").convertTo[Either[Int, String]] shouldEqual Right(
        "Hello"
      )
    }
  }

}
