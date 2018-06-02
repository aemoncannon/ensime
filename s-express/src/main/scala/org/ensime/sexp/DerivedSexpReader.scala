// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import scala.collection.immutable.ListMap

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

trait DerivedSexpReader[Base, A] {
  def readFields(
    t: ListMap[SexpSymbol, Sexp]
  ): Either[DeserializationException, A]
}
object DerivedSexpReader {

  def gen[A, Repr](
    implicit
    C: SexpConfig[A],
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedSexpReader[A, Repr]]]
  ): SexpReader[A] = {
    case (s: SexpSymbol) =>
      CR.value.value.readFields(ListMap(s -> SexpNil)).map(G.from)
    case SexpData(m) => CR.value.value.readFields(m).map(G.from)
    case x           => Left(DeserializationException(x))
  }

  implicit def hnil[A]: DerivedSexpReader[A, HNil] = (_ => Right(HNil))
  implicit def hcons[A, Key <: Symbol, Value, Remaining <: HList](
    implicit
    C: SexpConfig[A],
    Key: Witness.Aux[Key],
    LV: Lazy[SexpReader[Value]],
    DR: DerivedSexpReader[A, Remaining]
  ): DerivedSexpReader[A, FieldType[Key, Value] :*: Remaining] = { m =>
    val key = C.hint(Key.value.name)
    for {
      head <- LV.value.read(m.getOrElse(key, SexpNil))
      tail <- DR.readFields(m)
    } yield field[Key](head) :: tail
  }

  implicit def cnil[A]: DerivedSexpReader[A, CNil] =
    (_ => Left(DeserializationException(SexpNil)))
  implicit def ccons[A, Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    C: SexpConfig[A],
    Name: Witness.Aux[Name],
    LI: Lazy[SexpReader[Instance]],
    DR: DerivedSexpReader[A, Remaining]
  ): DerivedSexpReader[A, FieldType[Name, Instance] :+: Remaining] = { obj =>
    val key = Name.value.name
    obj.get(C.hint(key)) match {
      case None        => DR.readFields(obj).map(Inr.apply)
      case Some(value) => LI.value.read(value).map(v => Inl(field(v)))
    }
  }

}
