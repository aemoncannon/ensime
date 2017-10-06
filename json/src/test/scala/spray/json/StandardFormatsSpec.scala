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

  "The tuple1Format" should {
    "convert (42) to a JsNumber" in {
      Tuple1(42).toJson shouldEqual JsNumber(42)
    }
    "be able to convert a JsNumber to a Tuple1[Int]" in {
      JsNumber(42).convertTo[Tuple1[Int]] shouldEqual Tuple1(42)
    }
  }

  "The tuple2Format" should {
    val json = JsArray(JsNumber(42), JsNumber(4.2))
    "convert (42, 4.2) to a JsArray" in {
      (42, 4.2).toJson shouldEqual json
    }
    "be able to convert a JsArray to a (Int, Double)]" in {
      json.convertTo[(Int, Double)] shouldEqual ((42, 4.2))
    }
  }

  "The tuple3Format" should {
    val json = JsArray(JsNumber(42), JsNumber(4.2), JsNumber(3))
    "convert (42, 4.2, 3) to a JsArray" in {
      (42, 4.2, 3).toJson shouldEqual json
    }
    "be able to convert a JsArray to a (Int, Double, Int)]" in {
      json.convertTo[(Int, Double, Int)] shouldEqual ((42, 4.2, 3))
    }
  }
  "The tuple4Format" should {
    val json = JsArray(JsNumber(42), JsNumber(4.2), JsNumber(3), JsNumber(4))
    "convert (42, 4.2, 3, 4) to a JsArray" in {
      (42, 4.2, 3, 4).toJson shouldEqual json
    }
    "be able to convert a JsArray to a (Int, Double, Int, Int)]" in {
      json.convertTo[(Int, Double, Int, Int)] shouldEqual ((42, 4.2, 3, 4))
    }
  }
  "The tuple5Format" should {
    val json = JsArray(JsNumber(42),
                       JsNumber(4.2),
                       JsNumber(3),
                       JsNumber(4),
                       JsNumber(5))
    "convert (42, 4.2, 3, 4, 5) to a JsArray" in {
      (42, 4.2, 3, 4, 5).toJson shouldEqual json
    }
    "be able to convert a JsArray to a (Int, Double, Int, Int, Int)]" in {
      json.convertTo[(Int, Double, Int, Int, Int)] shouldEqual ((42,
                                                                 4.2,
                                                                 3,
                                                                 4,
                                                                 5))
    }
  }
  "The tuple6Format" should {
    val json = JsArray(JsNumber(42),
                       JsNumber(4.2),
                       JsNumber(3),
                       JsNumber(4),
                       JsNumber(5),
                       JsNumber(6))
    "convert (42, 4.2, 3, 4, 5, 6) to a JsArray" in {
      (42, 4.2, 3, 4, 5, 6).toJson shouldEqual json
    }
    "be able to convert a JsArray to a (Int, Double, Int, Int, Int, Int)]" in {
      json.convertTo[(Int, Double, Int, Int, Int, Int)] shouldEqual ((42,
                                                                      4.2,
                                                                      3,
                                                                      4,
                                                                      5,
                                                                      6))
    }
  }
  "The tuple7Format" should {
    val json = JsArray(JsNumber(42),
                       JsNumber(4.2),
                       JsNumber(3),
                       JsNumber(4),
                       JsNumber(5),
                       JsNumber(6),
                       JsNumber(7))
    "convert (42, 4.2, 3, 4, 5, 6, 7) to a JsArray" in {
      (42, 4.2, 3, 4, 5, 6, 7).toJson shouldEqual json
    }
    "be able to convert a JsArray to a (Int, Double, Int, Int, Int, Int, Int)]" in {
      json.convertTo[(Int, Double, Int, Int, Int, Int, Int)] shouldEqual ((42,
                                                                           4.2,
                                                                           3,
                                                                           4,
                                                                           5,
                                                                           6,
                                                                           7))
    }
  }
}
