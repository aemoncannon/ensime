// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray

import shapeless._

package object json {
  type JsField = (String, JsValue)

  def deserializationError(msg: String,
                           cause: Throwable = null,
                           fieldNames: List[String] = Nil) =
    throw new DeserializationException(msg, cause, fieldNames)
  def serializationError(msg: String) = throw new SerializationException(msg)

  implicit class EnrichedAny[T](val any: T) extends AnyVal {
    def toJson(implicit writer: JsonWriter[T]): JsValue = writer.write(any)
  }

  implicit class EnrichedString(val string: String) extends AnyVal {
    def parseJson: JsValue = JsonParser(string)
  }

  // slightly better alternatives to the xError methods above
  @inline
  def deserError[T: Typeable](msg: String, cause: Throwable = null): Nothing =
    throw new DeserializationException(
      s"deserialising ${Typeable[T].describe}: $msg",
      cause
    )

  @inline
  def unexpectedJson[T: Typeable](got: JsValue): Nothing =
    deserializationError(s"unexpected $got")

  @inline
  def serError[T: Typeable](msg: String): Nothing =
    throw new SerializationException(
      s"serialising ${Typeable[T].describe}: $msg"
    )
}

package json {

  case class DeserializationException(msg: String,
                                      cause: Throwable = null,
                                      fieldNames: List[String] = Nil)
      extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)
}
