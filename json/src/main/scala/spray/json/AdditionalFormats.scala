// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/**
 * Provides additional JsonFormats and helpers
 */
trait AdditionalFormats {

  implicit val jsValue: JsonFormat[JsValue] =
    JsonFormat.instance[JsValue](identity)(identity)

  implicit val jsObject: RootJsonFormat[JsObject] =
    RootJsonFormat.instance[JsObject](
      identity
    ) {
      case o: JsObject => o
      case _           => deserError[JsObject]("expected JsObject")
    }

  implicit val jsArray: RootJsonFormat[JsArray] =
    RootJsonFormat.instance[JsArray](identity) {
      case x: JsArray => x
      case _          => deserializationError("JSON array expected")
    }

}
