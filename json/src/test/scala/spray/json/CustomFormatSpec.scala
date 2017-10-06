// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import org.scalatest._
import Matchers._

class CustomFormatSpec extends WordSpec with DefaultJsonProtocol {

  case class MyType(name: String, value: Int)

  implicit val MyTypeProtocol = new RootJsonFormat[MyType] {
    def read(json: JsValue) = json match {
      case JsObject(fields) =>
        (fields.get("name"), fields.get("value")) match {
          case (Some(JsString(name)), Some(JsNumber(value))) =>
            MyType(name, value.toInt)
          case _ =>
            deserializationError(
              "Expected fields: 'name' (JSON string) and 'value' (JSON number)"
            )
        }
      case _ => deserError[MyType]("expected JsObject")
    }
    def write(obj: MyType) =
      JsObject("name" -> JsString(obj.name), "value" -> JsNumber(obj.value))
  }

  "A custom JsonFormat built with 'asJsonObject'" should {
    val value = MyType("bob", 42)
    "correctly deserialize valid JSON content" in {
      """{ "name": "bob", "value": 42 }""".parseJson
        .convertTo[MyType] shouldEqual value
    }
    "support full round-trip (de)serialization" in {
      value.toJson.convertTo[MyType] shouldEqual value
    }
  }

}
