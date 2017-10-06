// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import java.lang.StringBuilder

/**
 * A JsonPrinter that produces compact JSON source without any superfluous whitespace.
 */
trait CompactPrinter extends JsonPrinter {

  def print(x: JsValue, sb: StringBuilder): Unit =
    x match {
      case JsObject(x) => printObject(x, sb)
      case JsArray(x)  => printArray(x, sb)
      case _           => printLeaf(x, sb)
    }

  protected def printObject(members: Map[String, JsValue],
                            sb: StringBuilder): Unit = {
    sb.append('{')
    printSeq(members, sb.append(',')) { m =>
      printString(m._1, sb)
      sb.append(':')
      print(m._2, sb)
    }
    sb.append('}')
  }

  protected def printArray(elements: Seq[JsValue], sb: StringBuilder): Unit = {
    sb.append('[')
    printSeq(elements, sb.append(','))(print(_, sb))
    sb.append(']')
  }
}

object CompactPrinter extends CompactPrinter
