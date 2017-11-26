// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import org.scalatest._
import JsReader.ops._
import JsWriter.ops._

trait SprayJsonTestSupport {
  this: Matchers =>

  def roundtrip[T: JsReader: JsWriter](value: T,
                                       via: Option[String] = None): Unit = {
    val json = value.toJson

    via match {
      case None =>
        println(
          s"check and add the following assertion: $value = ${PrettyPrinter(json)}"
        )
      case Some(expected) => json shouldBe JsParser(expected)
    }

    val recovered = json.as[T]
    recovered shouldBe value
  }

  def roundtrip[T: JsReader: JsWriter](value: T, via: String): Unit =
    roundtrip(value, Some(via))

}
