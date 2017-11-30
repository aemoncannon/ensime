// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

/**
 * Emacs flavoured lisp printer.
 */
trait SexpPrinter extends (Sexp => String) {

  /**
   * Convert the input to a `String` (constructing the entire `String`
   * in memory).
   */
  def apply(x: Sexp): String = {
    val sb = new StringBuilder
    print(x, sb)
    sb.toString()
  }

  /**
   * Convert the input to a `String` with a swank message title.
   */
  def apply(x: Sexp, swank: String, rpc: Long): String = {
    val sb = new StringBuilder
    print(x, swank, rpc, sb)
    sb.toString()
  }

  /**
   * Perform the side effect of rendering `x` to `sb`, wrapping in a
   * swank message title and RPC id.
   */
  def print(x: Sexp, swank: String, rpc: Long, sb: StringBuilder): Unit = {
    sb.append('(').append(swank).append(' ')
    print(x, sb)
    sb.append(' ').append(rpc).append(')')
  }

  /**
   * Perform the side effect of rendering `x` to `sb`, which may
   * reduce memory requirements when rendering very large objects.
   */
  def print(x: Sexp, sb: StringBuilder): Unit

  protected def printAtom(sexp: SexpAtom, sb: StringBuilder): Unit =
    sexp match {
      case SexpChar(c)       => sb.append('?').append(c)
      case SexpSymbol(s)     => printSymbol(s, sb)
      case SexpString(s)     => printString(s, sb)
      case SexpNil           => sb.append("nil")
      case SexpInteger(long) => sb.append(long.toString)
      case SexpFloat(n) =>
        if (n.isNaN) sb.append("0.0e+NaN")
        else if (n == Double.PositiveInfinity) sb.append("1.0e+INF")
        else if (n == Double.NegativeInfinity) sb.append("-1.0e+INF")
        else sb.append(n.toString)
    }

  // we prefer not to escape some characters when in strings
  private val exclude  = Set("\n", "\t", " ")
  private val specials = SexpParser.specialChars.toList.map(_.swap)
  private val stringSpecials =
    SexpParser.specialChars.toList.map(_.swap).filterNot {
      case (from, to) => exclude(from)
    }

  protected def printSymbol(s: String, sb: StringBuilder): Unit = {
    val escaped = specials.foldLeft(s) {
      case (r, (from, to)) => r.replace(from, "\\" + to)
    }
    sb.append(escaped)
  }

  protected def printString(s: String, sb: StringBuilder): Unit = {
    val escaped = stringSpecials.foldLeft(s) {
      case (r, (from, to)) => r.replace(from, "\\" + to)
    }
    sb.append('"').append(escaped).append('"')
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

object SexpCompactPrinter extends SexpPrinter {

  def print(sexp: Sexp, sb: StringBuilder): Unit = sexp match {
    case atom: SexpAtom => printAtom(atom, sb)
    case SexpData(data) => printData(data, sb)
    case SexpList(els)  => printList(els, sb)
    case SexpCons(x, y) => printCons(x, y, sb)
  }

  protected def printCons(x: Sexp, y: Sexp, sb: StringBuilder): Unit = {
    // recursive, could blow up for big trees
    sb.append('(')
    print(x, sb)
    sb.append(" . ")
    print(y, sb)
    sb.append(')')
  }

  protected def printData(data: Map[SexpSymbol, Sexp],
                          sb: StringBuilder): Unit =
    if (data.isEmpty) printAtom(SexpNil, sb)
    else {
      sb.append('(')
      printSeq(data, sb.append(' ')) { el =>
        printSymbol(el._1.value, sb)
        sb.append(' ')
        print(el._2, sb)
      }
      sb.append(')')
    }

  protected def printList(els: List[Sexp], sb: StringBuilder): Unit =
    if (els.isEmpty) printAtom(SexpNil, sb)
    else {
      sb.append('(')
      printSeq(els, sb.append(' ')) {
        print(_, sb)
      }
      sb.append(')')
    }

}

/**
 * The output is a non-standard interpretation of "pretty lisp" ---
 * emacs style formatting requires counting the length of the text on
 * the current line and indenting off that, which is not so easy when
 * all you have is a `StringBuilder`.
 */
object SexpPrettyPrinter extends SexpPrinter {
  val Indent = 2

  def print(sexp: Sexp, sb: StringBuilder): Unit = print(sexp, sb, 0)

  private def print(sexp: Sexp, sb: StringBuilder, indent: Int): Unit =
    sexp match {
      case SexpData(data) => printData(data, sb, indent)
      case SexpList(els)  => printList(els, sb, indent)
      case SexpCons(x, y) => printCons(x, y, sb, indent)
      case atom: SexpAtom => printAtom(atom, sb)
    }

  protected def printCons(x: Sexp,
                          y: Sexp,
                          sb: StringBuilder,
                          indent: Int): Unit = {
    // recursive, could blow up for big trees
    sb.append('(')
    print(x, sb, indent)
    sb.append(" .\n")
    printIndent(sb, indent + Indent)
    print(y, sb, indent + Indent)
    sb.append(')')
  }

  protected def printData(data: Map[SexpSymbol, Sexp],
                          sb: StringBuilder,
                          indent: Int): Unit =
    if (data.isEmpty) printAtom(SexpNil, sb)
    else {
      sb.append("(\n")
      printSeq(data, sb.append('\n')) { el =>
        printIndent(sb, indent + Indent)
        printSymbol(el._1.value, sb)
        sb.append(' ')
        print(el._2, sb, indent + Indent)
      }
      sb.append('\n')
      printIndent(sb, indent)
      sb.append(')')
    }

  protected def printList(els: List[Sexp],
                          sb: StringBuilder,
                          indent: Int): Unit =
    if (els.isEmpty) printAtom(SexpNil, sb)
    else {
      sb.append('(')
      printSeq(els, { sb.append("\n"); printIndent(sb, indent + Indent) }) {
        print(_, sb, indent + Indent)
      }
      sb.append(')')
    }

  protected def printIndent(sb: StringBuilder, indent: Int): Unit =
    (0 until indent) foreach { _ =>
      sb.append(' ')
    }

}
