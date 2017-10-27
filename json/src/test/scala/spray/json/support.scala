// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import org.scalatest._

trait SprayJsonTestSupport {
  this: Matchers =>

  def roundtrip[T: JsonFormat](value: T, via: Option[String] = None): Unit = {
    val json   = value.toJson
    val string = json.compactPrint

    via match {
      case None =>
        println(s"check and add the following assertion: $value = $string")
      case Some(expected) => string shouldBe expected
    }

    val recovered = json.convertTo[T]
    recovered shouldBe value
  }
  def roundtrip[T: JsonFormat](value: T, via: String): Unit =
    roundtrip(value, Some(via))

}
