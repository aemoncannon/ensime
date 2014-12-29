package org.ensime.sexp.formats

import collection.GenTraversableOnce
import collection.generic.CanBuildFrom
import collection.breakOut
import collection.{ immutable => im, mutable => mut }

import org.ensime.sexp._
import scala.collection.GenMap
import scala.collection.GenTraversable

/**
 * Support for anything with a `CanBuildFrom`.
 */
trait CollectionFormats {
  this: BasicFormats =>

  /*
   Implementation note. Ideally, and intuitively, we'd like to be able
   to write the type signature of this implicit method as

     getTraversableformat[E, T <: GenTraversable[E]](implicit
       cbf: CanBuildFrom[T, E, T],
       ef:  SexpFormat[E]
     )

   but due to limitations of the Scala compiler, the "kindedness" of a
   type parameter restricts what it can be equated to. A no-param type
   such as `T` (of `*` kind) cannot be equated to a one-param type
   such as `T[E]` (of `* -> *` kind), which cannot be equated to a
   two-param type such as `T[K, V]` (of `* -> * -> *` kind).

   This deficiency is tracked under
   [SI-2712](https://issues.scala-lang.org/browse/SI-2712)
   "implement higher-order unification for type constructor inference".

   The workaround is to define the types with the correct kindedness
   (using free type parameters) and then introducing implicit evidence
   to restrict the free type parameters (note that `<:<` is defined in
   Predef).

   SIDENOTE: An alternative way of implementing genTraversableFormat
   would be to define it using a refinement:

     genTraversbaleFormat[E, T](implicit
       itl: IsTraversableLike[T] { type A = E },
       cbf: CanBuildFrom[T, E, T],
       ef: SexpFormat[E]
     )

   but this only works because `IsTraversableLike` is using the evidence
   trick under the hood.
   */
  implicit def genTraversableFormat[T[_], E](
    implicit evidence: T[E] <:< GenTraversable[E],
    cbf: CanBuildFrom[T[E], E, T[E]],
    ef: SexpFormat[E]): SexpFormat[T[E]] = new SexpFormat[T[E]] {
    def write(t: T[E]) = SexpList(t.map(_.toSexp)(breakOut): List[Sexp])

    def read(v: Sexp): T[E] = v match {
      case SexpNil => cbf().result()
      case SexpList(els) => els.map(_.convertTo[E])(breakOut)
      case x => deserializationError(x)
    }
  }

  /*
   We could potentially have a specialised mapDataFormat (using
   `SexpData`) if we know that the keys can be converted into
   `SexpSymbol`, but that would overcomplicate the `SexpFormat`
   hierarchy.
   */
  implicit def genMapFormat[M[_, _], K, V](
    implicit ev: M[K, V] <:< GenMap[K, V],
    cbf: CanBuildFrom[M[K, V], (K, V), M[K, V]],
    kf: SexpFormat[K],
    vf: SexpFormat[V]): SexpFormat[M[K, V]] = new SexpFormat[M[K, V]] {
    def write(m: M[K, V]) =
      SexpList(m.map {
        case (k, v) => SexpList(k.toSexp, v.toSexp)
      }(breakOut): List[Sexp]
      )

    def read(v: Sexp): M[K, V] = v match {
      case SexpNil => cbf().result
      case SexpList(els) => els.map {
        case SexpList(sk :: sv :: Nil) => (sk.convertTo[K], sv.convertTo[V])
        case x => deserializationError(x)
      }(breakOut)
      case x => deserializationError(x)
    }
  }

  private val start = SexpSymbol(":start")
  private val end = SexpSymbol(":end")
  private val step = SexpSymbol(":step")
  private val inclusive = SexpSymbol(":inclusive")
  implicit object RangeFormat extends SexpFormat[im.Range] {
    def write(r: im.Range) = SexpData(
      start -> SexpNumber(r.start),
      end -> SexpNumber(r.end),
      step -> SexpNumber(r.step))

    def read(s: Sexp) = s match {
      case SexpData(data) =>
        (data(start), data(end), data(step)) match {
          case (SexpNumber(s), SexpNumber(e), SexpNumber(st)) =>
            Range(s.toInt, e.toInt, st.toInt)
          case _ => deserializationError(s)
        }
      case _ => deserializationError(s)
    }
  }

  // note that the type has to be im.NumericRange[E]
  // not im.NumericRange.{Inclusive, Exclusive}[E]
  // (same problem as above, but getting the cons is trickier)
  implicit def numericRangeFormat[E](implicit nf: SexpFormat[E],
    n: Numeric[E],
    int: Integral[E]): SexpFormat[im.NumericRange[E]] = new SexpFormat[im.NumericRange[E]] {
    def write(r: im.NumericRange[E]) = SexpData(
      start -> r.start.toSexp,
      end -> r.end.toSexp,
      step -> r.step.toSexp,
      inclusive -> BooleanFormat.write(r.isInclusive)
    )

    def read(s: Sexp): im.NumericRange[E] = s match {
      case SexpData(data) =>
        (data(start), data(end), data(step), data(inclusive)) match {
          case (s, e, st, incl) if BooleanFormat.read(incl) =>
            im.NumericRange.inclusive(
              s.convertTo[E], e.convertTo[E], st.convertTo[E])
          case (s, e, st, incl) =>
            im.NumericRange(s.convertTo[E], e.convertTo[E], st.convertTo[E])
          case _ => deserializationError(s)
        }
      case _ => deserializationError(s)
    }
  }

}

