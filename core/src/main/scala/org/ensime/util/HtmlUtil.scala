// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import scala.util.Try

object HtmlUtil {

  def unescapeHtml(escaped: String): Option[String] =
    Try {
      val result = escaped.foldLeft[(String, Option[String])](("", None)) {
        case ((acc, escapedElemAcc), c) =>
          (c, escapedElemAcc) match {
            case ('&', None) =>
              (acc, Some(""))
            case (_, None) =>
              (acc + c, None)
            case ('&', Some(_)) =>
              throw new IllegalArgumentException(
                "nested escape sequences not supported"
              )
            case (';', Some(escapedElem)) =>
              (acc + unescapeMap(escapedElem), None)
            case (_, Some(incompleteEscapedElem)) =>
              (acc, Some(incompleteEscapedElem + c))
          }
      }
      result match {
        case (escaped, None) =>
          escaped
        case _ =>
          throw new IllegalArgumentException(
            "unfinished escape sequence not supported"
          )
      }
    }.toOption

  //Minimal unescape map based on scala.xml.Utility.unescape() should be enough for most HTML
  private val unescapeMap = Map(
    "lt"   -> '<',
    "gt"   -> '>',
    "amp"  -> '&',
    "quot" -> '"',
    "apos" -> '\''
  )
}
