// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import org.scalatest._
import Matchers._

import JsWriter.ops._
import JsReader.ops._

class CollectionFormatsSpec extends WordSpec {

  "The listFormat" should {
    val list = List(1, 2, 3)
    val json = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert a List[Int] to a JsArray of JsNumbers" in {
      list.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to a List[Int]" in {
      json.as[List[Int]] shouldEqual Right(list)
    }
  }

  /*
  "The arrayFormat" should {
    import java.util.Arrays
    val array = Array(1, 2, 3)
    val json  = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert an Array[Int] to a JsArray of JsNumbers" in {
      array.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to an Array[Int]" in {
      Arrays.equals(json.as[Array[Int]], array) shouldBe true
    }
  }*/

  "The mapFormat" should {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val json =
      JsObject("a" -> JsNumber(1), "b" -> JsNumber(2), "c" -> JsNumber(3))
    "convert a Map[String, Long] to a JsObject" in {
      map.toJson shouldEqual json
    }
    "be able to convert a JsObject to a Map[String, Long]" in {
      json.as[Map[String, Long]] shouldEqual Right(map)
    }
  }

  "The immutableSetFormat" should {
    val set  = Set(1, 2, 3)
    val json = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert a Set[Int] to a JsArray of JsNumbers" in {
      set.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to a Set[Int]" in {
      json.as[Set[Int]] shouldEqual Right(set)
    }
  }

  "The indexedSeqFormat" should {
    val seq  = collection.IndexedSeq(1, 2, 3)
    val json = JsArray(JsNumber(1), JsNumber(2), JsNumber(3))
    "convert a Set[Int] to a JsArray of JsNumbers" in {
      seq.toJson shouldEqual json
    }
    "convert a JsArray of JsNumbers to a IndexedSeq[Int]" in {
      json.as[collection.IndexedSeq[Int]] shouldEqual Right(seq)
    }
  }

}
