// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import org.scalatest._
import Matchers._

class AdditionalFormatsSpec extends WordSpec {

  case class Container[A](inner: Option[A])

  object ReaderProtocol extends DefaultJsonProtocol {
    implicit def containerReader[T: JsonFormat] = lift {
      new JsonReader[Container[T]] {
        def read(value: JsValue) = value match {
          case JsObject(fields) if fields.contains("content") =>
            Container(Some(JsonReader[T].read(fields("content"))))
          case _ => deserializationError("Unexpected format: " + value.toString)
        }
      }
    }
  }

  object WriterProtocol extends DefaultJsonProtocol {
    implicit def containerWriter[T: JsonFormat] = lift {
      new JsonWriter[Container[T]] {
        def write(obj: Container[T]) = JsObject("content" -> obj.inner.toJson)
      }
    }
  }

  "The liftJsonWriter" should {
    val obj = Container(Some(Container(Some(List(1, 2, 3)))))

    "properly write a Container[Container[List[Int]]] to JSON" in {
      import WriterProtocol._
      obj.toJson.toString shouldEqual """{"content":{"content":[1,2,3]}}"""
    }

    "properly read a Container[Container[List[Int]]] from JSON" in {
      import ReaderProtocol._
      """{"content":{"content":[1,2,3]}}""".parseJson
        .convertTo[Container[Container[List[Int]]]] shouldEqual obj
    }
  }

}
