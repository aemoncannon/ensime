// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import java.lang.StringBuilder
import annotation.tailrec

/**
 * A JsonPrinter that produces a nicely readable JSON source.
 */
trait PrettyPrinter extends JsonPrinter {
  val Indent = 2

  def print(x: JsValue, sb: StringBuilder): Unit =
    print(x, sb, 0)

  protected def print(x: JsValue, sb: StringBuilder, indent: Int): Unit =
    x match {
      case JsObject(x) => printObject(x, sb, indent)
      case JsArray(x)  => printArray(x, sb, indent)
      case _           => printLeaf(x, sb)
    }

  protected def printObject(members: Map[String, JsValue],
                            sb: StringBuilder,
                            indent: Int): Unit = {
    sb.append("{\n")
    printSeq(members, sb.append(",\n")) { m =>
      printIndent(sb, indent + Indent)
      printString(m._1, sb)
      sb.append(": ")
      print(m._2, sb, indent + Indent)
    }
    sb.append('\n')
    printIndent(sb, indent)
    sb.append("}")
  }

  protected def printArray(elements: Seq[JsValue],
                           sb: StringBuilder,
                           indent: Int): Unit = {
    sb.append('[')
    printSeq(elements, sb.append(", "))(print(_, sb, indent))
    sb.append(']')
  }

  protected def printIndent(sb: StringBuilder, indent: Int): Unit = {
    @tailrec def rec(indent: Int): Unit =
      if (indent > 0) {
        sb.append(' ')
        rec(indent - 1)
      }
    rec(indent)
  }
}

object PrettyPrinter extends PrettyPrinter
