// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import scala.collection.immutable.ListMap

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

trait DerivedSexpWriter[Base, A] {
  def writeFields(a: A): Either[SexpSymbol, List[(SexpSymbol, Sexp)]]
}
object DerivedSexpWriter {

  def gen[A, Repr](
    implicit
    C: SexpConfig[A],
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedSexpWriter[A, Repr]]]
  ): SexpWriter[A] = new SexpWriter[A] {
    final override def write(a: A): Sexp =
      CR.value.value.writeFields(G.to(a)) match {
        case Left(sym)   => sym //SexpCons(sym, SexpNil)
        case Right(data) => SexpData(data)
      }
  }

  implicit def hnil[A]: DerivedSexpWriter[A, HNil] = (_ => Right(Nil))
  implicit def hcons[A, Key <: Symbol, Value, Remaining <: HList](
    implicit
    C: SexpConfig[A],
    Key: Witness.Aux[Key],
    LV: Lazy[SexpWriter[Value]],
    DR: DerivedSexpWriter[A, Remaining]
  ): DerivedSexpWriter[A, FieldType[Key, Value] :*: Remaining] = {
    case head :*: tail =>
      DR.writeFields(tail).map { other =>
        LV.value.write(head) match {
          case SexpNil => other
          case value   => (C.hint(Key.value.name) -> value) :: other
        }
      }
  }

  implicit def cnil[A]: DerivedSexpWriter[A, CNil] = (_ => Right(Nil))
  implicit def ccons[A, Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    C: SexpConfig[A],
    Name: Witness.Aux[Name],
    LI: Lazy[SexpWriter[Instance]],
    DR: DerivedSexpWriter[A, Remaining]
  ): DerivedSexpWriter[A, FieldType[Name, Instance] :+: Remaining] = {
    case Inl(ins) =>
      val key = C.hint(Name.value.name)
      LI.value.write(ins) match {
        case SexpNil => Left(key)
        case value   => Right(key -> value :: Nil)
      }
    case Inr(rem) => DR.writeFields(rem)
  }

}
