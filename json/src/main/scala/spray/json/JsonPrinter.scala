// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import annotation.tailrec
import java.lang.{ StringBuilder => JStringBuilder }

/**
 * A JsonPrinter serializes a JSON AST to a String.
 */
trait JsonPrinter extends (JsValue => String) {

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
      case JsNull      => sb.append("null")
      case JsTrue      => sb.append("true")
      case JsFalse     => sb.append("false")
      case JsNumber(x) => sb.append(x)
      case JsString(x) => printString(x, sb)
      case _           => throw new IllegalStateException
    }

  protected def printString(s: String, sb: JStringBuilder): Unit = {
    import JsonPrinter._
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

object JsonPrinter {
  def requiresEncoding(c: Char): Boolean =
    // from RFC 4627
    // unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    c match {
      case '"'  => true
      case '\\' => true
      case c    => c < 0x20
    }
}
