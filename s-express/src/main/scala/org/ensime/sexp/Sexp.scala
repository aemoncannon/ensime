// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import collection.breakOut
import scala.collection.immutable.ListMap

/**
 * An S-Expression is either
 *
 * 1. an atom (i.e. symbol, string, number)
 * 2. of the form `(x . y)` where `x` and `y` are S-Expressions (i.e. cons)
 *
 * Everything else is just sugar.
 */
sealed abstract class Sexp

sealed trait SexpList                       extends Sexp
final case class SexpCons(x: Sexp, y: Sexp) extends SexpList

sealed abstract class SexpAtom             extends Sexp
final case class SexpChar(value: Char)     extends SexpAtom
final case class SexpString(value: String) extends SexpAtom

// https://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html
final case class SexpInteger(value: Long) extends SexpAtom

// https://www.gnu.org/software/emacs/manual/html_node/elisp/Float-Basics.html
final case class SexpFloat(value: Double) extends SexpAtom

final case class SexpSymbol(value: String) extends SexpAtom
case object SexpNil                        extends SexpAtom with SexpList

/** Sugar for ("a" . ("b" . ("c" . nil))) */
object SexpList {
  def apply(els: Sexp*): Sexp = apply(els.toList)

  def apply(els: List[Sexp]): Sexp = els.foldRight(SexpNil: Sexp) {
    case (head, tail) => SexpCons(head, tail)
  }

  def unapply(sexp: Sexp): Option[List[Sexp]] = sexp match {
    case list: SexpList =>
      def rec(s: SexpList): Option[List[Sexp]] = s match {
        case SexpNil                      => Some(Nil)
        case SexpCons(car, cdr: SexpList) => rec(cdr).map(car :: _)
        case _                            => None
      }
      rec(list)
    case _ => None
  }
}

/**
 * Sugar for (:k1 v1 :k2 v2)
 * [keyword symbols](https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html):
 *
 * `SexpData` is shorthand for `ListMap[SexpSymbol, Sexp]`
 */
object SexpData {
  def apply(kvs: (SexpSymbol, Sexp)*): Sexp = apply(kvs.toList)

  // not total...
  def apply(kvs: List[(SexpSymbol, Sexp)]): Sexp =
    if (kvs.isEmpty)
      SexpNil
    else {
      val mapped = ListMap(kvs: _*)
      require(mapped.size == kvs.size,
              "duplicate keys not allowed: " + mapped.keys)
      require(mapped.keys.forall(_.value.startsWith(":")),
              "keys must start with ':' " + mapped.keys)
      SexpList(
        kvs.flatMap { case (k, v) => k :: v :: Nil }(breakOut): List[Sexp]
      )
    }

  def unapply(sexp: Sexp): Option[ListMap[SexpSymbol, Sexp]] = sexp match {
    case SexpList(values) =>
      // order can be important in serialised forms
      val props = {
        values.grouped(2).collect {
          case List(SexpSymbol(key), value) if key.startsWith(":") =>
            (SexpSymbol(key), value)
        }
      }.foldLeft(ListMap.empty[SexpSymbol, Sexp]) {
        case (res, el) =>
          // in elisp, first entry wins
          if (res.contains(el._1)) res else res + el
      }
      // props.size counts unique keys. We only create data when keys
      // are not duplicated or we could introduce losses
      if (2 * props.size != values.size)
        None
      else
        Some(props)

    case _ => None
  }
}
