// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.jerky

import java.io.File

import org.scalatest._
import spray.json._

import JsWriter.ops._
import JsReader.ops._

import org.ensime.api.RawFile

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

object EscapingStringInterpolation {

  /**
   * String interpolation that automatically escapes known "bad" types (such as
   * `File` on Windows) and *ONLY* for use in ENSIME tests when asserting on
   * wire formats.
   */
  final case class StringContext(parts: String*) {
    private val delegate = new scala.StringContext(parts: _*)
    def s(args: Any*): String = {
      val hijacked = args.map {
        case f: File       => f.toString.replace("""\""", """\\""")
        case RawFile(path) => path.toFile.toString.replace("""\""", """\\""")
        case other         => other
      }
      delegate.s(hijacked: _*)
    }
  }
}
