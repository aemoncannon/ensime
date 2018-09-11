// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import shapeless._

/**
 * This file exists because the JsReader returns A rather than String \/ A. We
 * should fix this.
 */
final case class DeserializationException(msg: String,
                                          cause: Throwable = null,
                                          fieldNames: List[String] = Nil)
object DeserializationException {
  @inline
  def deserError[T: Typeable](
    msg: String,
    cause: Throwable = null
  ): DeserializationException =
    DeserializationException(s"deserialising ${Typeable[T].describe}: $msg",
                             cause)

  @inline
  def unexpectedJson[T: Typeable](got: JsValue): DeserializationException =
    DeserializationException(s"unexpected $got")

}
