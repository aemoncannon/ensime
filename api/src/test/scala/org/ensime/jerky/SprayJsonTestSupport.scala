// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.jerky

import org.scalatest._
import org.scalactic.source.Position
import spray.json._

import JsWriter.ops._
import JsReader.ops._

trait SprayJsonTestSupport {
  this: Matchers =>

  def roundtrip[T: JsReader: JsWriter](value: T, via: Option[String] = None)(
    implicit p: Position
  ): Unit = {
    val json = value.toJson

    via match {
      case None =>
        println(
          s"check and add the following assertion: $value = ${PrettyPrinter(json)}"
        )
      case Some(expected) => json shouldBe JsParser(expected)
    }

    val recovered = json.as[T]
    recovered shouldBe Right(value)
  }

  def roundtrip[T: JsReader: JsWriter](value: T, via: String)(
    implicit p: Position
  ): Unit =
    roundtrip(value, Some(via))

}
