// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import scala.collection.immutable.ListMap

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

trait DerivedSexpReader[Base, A] {
  def readFields(t: ListMap[SexpSymbol, Sexp]): A
}
object DerivedSexpReader {

  def gen[A, Repr](
    implicit
    C: SexpConfig[A],
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedSexpReader[A, Repr]]]
  ): SexpReader[A] = {
    case (s: SexpSymbol) =>
      G.from(CR.value.value.readFields(ListMap(s -> SexpNil)))
    case SexpData(m) => G.from(CR.value.value.readFields(m))
    case x           => throw new DeserializationException(x)
  }

  implicit def hnil[A]: DerivedSexpReader[A, HNil] = (_ => HNil)
  implicit def hcons[A, Key <: Symbol, Value, Remaining <: HList](
    implicit
    C: SexpConfig[A],
    Key: Witness.Aux[Key],
    LV: Lazy[SexpReader[Value]],
    DR: DerivedSexpReader[A, Remaining]
  ): DerivedSexpReader[A, FieldType[Key, Value] :*: Remaining] = { m =>
    val key  = C.hint(Key.value.name)
    val read = LV.value.read(m.getOrElse(key, SexpNil))
    field[Key](read) :: DR.readFields(m)
  }

  implicit def cnil[A]: DerivedSexpReader[A, CNil] =
    (_ => throw new DeserializationException(SexpNil))
  implicit def ccons[A, Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    C: SexpConfig[A],
    Name: Witness.Aux[Name],
    LI: Lazy[SexpReader[Instance]],
    DR: DerivedSexpReader[A, Remaining]
  ): DerivedSexpReader[A, FieldType[Name, Instance] :+: Remaining] = { obj =>
    val key = Name.value.name
    obj.get(C.hint(key)) match {
      case None        => Inr(DR.readFields(obj))
      case Some(value) => Inl(field[Name](LI.value.read(value)))
    }
  }

}
