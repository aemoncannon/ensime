// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import annotation.tailrec
import java.lang.{ StringBuilder => JStringBuilder }

/**
 * A JsPrinter serializes a JSON AST to a String.
 */
trait JsPrinter extends (JsValue => String) {

  def apply(x: JsValue): String = apply(x, None)

  def apply(
    x: JsValue,
    jsonpCallback: Option[String] = None,
    sb: JStringBuilder = new JStringBuilder(256)
  ): String = {
    jsonpCallback match {
      case Some(callback) =>
        sb.append(callback).append('(')
        print(x, sb)
        sb.append(')')
      case None => print(x, sb)
    }
    sb.toString
  }

  def print(x: JsValue, sb: JStringBuilder): Unit

  protected def printLeaf(x: JsValue, sb: JStringBuilder): Unit =
    x match {
      case JsNull           => sb.append("null")
      case JsBoolean(true)  => sb.append("true")
      case JsBoolean(false) => sb.append("false")
      case JsNumber(x)      => sb.append(x)
      case JsString(x)      => printString(x, sb)
      case _                => throw new IllegalStateException
    }

  protected def printString(s: String, sb: JStringBuilder): Unit = {
    import JsPrinter._
    @tailrec def firstToBeEncoded(ix: Int = 0): Int =
      if (ix == s.length) -1
      else if (requiresEncoding(s.charAt(ix))) ix
      else firstToBeEncoded(ix + 1)

    sb.append('"')
    firstToBeEncoded() match {
      case -1 ⇒ sb.append(s)
      case first ⇒
        sb.append(s, 0, first)
        @tailrec def append(ix: Int): Unit =
          if (ix < s.length) {
            s.charAt(ix) match {
              case c if !requiresEncoding(c) => sb.append(c)
              case '"'                       => sb.append("\\\"")
              case '\\'                      => sb.append("\\\\")
              case '\b'                      => sb.append("\\b")
              case '\f'                      => sb.append("\\f")
              case '\n'                      => sb.append("\\n")
              case '\r'                      => sb.append("\\r")
              case '\t'                      => sb.append("\\t")
              case x if x <= 0xF =>
                sb.append("\\u000").append(Integer.toHexString(x.toInt))
              case x if x <= 0xFF =>
                sb.append("\\u00").append(Integer.toHexString(x.toInt))
              case x if x <= 0xFFF =>
                sb.append("\\u0").append(Integer.toHexString(x.toInt))
              case x => sb.append("\\u").append(Integer.toHexString(x.toInt))
            }
            append(ix + 1)
          }
        append(first)
    }
    sb.append('"')
  }

  protected def printSeq[A](iterable: Iterable[A],
                            printSeparator: => Unit)(f: A => Unit): Unit = {
    var first = true
    iterable.foreach { a =>
      if (first) first = false else printSeparator
      f(a)
    }
  }
}

object JsPrinter {
  def requiresEncoding(c: Char): Boolean =
    // from RFC 4627
    // unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    c match {
      case '"'  => true
      case '\\' => true
      case c    => c < 0x20
    }
}

/**
 * A JsPrinter that produces compact JSON source without any superfluous whitespace.
 */
object CompactPrinter extends JsPrinter {

  def print(x: JsValue, sb: JStringBuilder): Unit =
    x match {
      case JsObject(x) => printObject(x, sb)
      case JsArray(x)  => printArray(x, sb)
      case _           => printLeaf(x, sb)
    }

  protected def printObject(members: Map[String, JsValue],
                            sb: JStringBuilder): Unit = {
    sb.append('{')
    printSeq(members, sb.append(',')) { m =>
      printString(m._1, sb)
      sb.append(':')
      print(m._2, sb)
    }
    sb.append('}')
  }

  protected def printArray(elements: Seq[JsValue], sb: JStringBuilder): Unit = {
    sb.append('[')
    printSeq(elements, sb.append(','))(print(_, sb))
    sb.append(']')
  }
}

/**
 * A JsPrinter that produces a nicely readable JSON source.
 */
object PrettyPrinter extends JsPrinter {
  val Indent = 2

  def print(x: JsValue, sb: JStringBuilder): Unit =
    print(x, sb, 0)

  protected def print(x: JsValue, sb: JStringBuilder, indent: Int): Unit =
    x match {
      case JsObject(x) => printObject(x, sb, indent)
      case JsArray(x)  => printArray(x, sb, indent)
      case _           => printLeaf(x, sb)
    }

  protected def printObject(members: Map[String, JsValue],
                            sb: JStringBuilder,
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
                           sb: JStringBuilder,
                           indent: Int): Unit = {
    sb.append('[')
    printSeq(elements, sb.append(", "))(print(_, sb, indent))
    sb.append(']')
  }

  protected def printIndent(sb: JStringBuilder, indent: Int): Unit = {
    @tailrec def rec(indent: Int): Unit =
      if (indent > 0) {
        sb.append(' ')
        rec(indent - 1)
      }
    rec(indent)
  }
}
