// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.api

import spray.json._
import types._

import scala.util.{ Failure, Success, Try }

private object TypesConversions extends DefaultJsonProtocol with FamilyFormats {

  implicit val markedStringFormat: JsonFormat[MarkedString] =
    new JsonFormat[MarkedString] {
      def read(j: JsValue): MarkedString =
        (Try(j.convertTo[RawMarkedString]) orElse
          Try(j.convertTo[MarkdownString])) match {
          case Failure(e) => sys.error(s"Wrong MarkedString format for $j")
          case Success(x) => x
        }

      override def write(obj: MarkedString): JsValue = obj match {
        case raw: RawMarkedString     => raw.toJson
        case markdown: MarkdownString => markdown.toJson
      }
    }
}
