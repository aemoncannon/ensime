// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import java.util.Arrays

import org.scalatest._
import Matchers._

class CollectionFormatsSpec extends WordSpec with DefaultJsonProtocol {

  "The listFormat" should {
    val list = List(1, 2, 3)
    val json = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert a List[Int] to a JsArray of JsNumbers" in {
      list.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to a List[Int]" in {
      json.convertTo[List[Int]] shouldEqual list
    }
  }

  "The arrayFormat" should {
    val array = Array(1, 2, 3)
    val json  = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert an Array[Int] to a JsArray of JsNumbers" in {
      array.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to an Array[Int]" in {
      Arrays.equals(json.convertTo[Array[Int]], array) shouldBe true
    }
  }

  "The mapFormat" should {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val json =
      JsObject("a" -> JsNumber(1), "b" -> JsNumber(2), "c" -> JsNumber(3))
    "convert a Map[String, Long] to a JsObject" in {
      map.toJson shouldEqual json
    }
    "be able to convert a JsObject to a Map[String, Long]" in {
      json.convertTo[Map[String, Long]] shouldEqual map
    }
    "throw an Exception when trying to serialize a map whose key are not serialized to JsStrings" in {
      intercept[SerializationException] {
        Map(1 -> "a").toJson
      }.getMessage shouldEqual "Map key must be formatted as JsString, not '1'"
    }
  }

  "The immutableSetFormat" should {
    val set  = Set(1, 2, 3)
    val json = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert a Set[Int] to a JsArray of JsNumbers" in {
      set.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to a Set[Int]" in {
      json.convertTo[Set[Int]] shouldEqual set
    }
  }

  "The indexedSeqFormat" should {
    val seq  = collection.IndexedSeq(1, 2, 3)
    val json = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert a Set[Int] to a JsArray of JsNumbers" in {
      seq.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to a IndexedSeq[Int]" in {
      json.convertTo[collection.IndexedSeq[Int]] shouldEqual seq
    }
  }

}
