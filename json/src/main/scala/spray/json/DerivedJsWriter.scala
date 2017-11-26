// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import scala.collection.immutable.ListMap

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

trait DerivedJsWriter[Base, A] {
  def writeFields(a: A): List[(String, JsValue)]
}
object DerivedJsWriter {

  def gen[A, Repr](
    implicit
    C: JsConfig[A],
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedJsWriter[A, Repr]]]
  ): JsWriter[A] = new JsWriter[A] {
    final override def write(a: A): JsValue =
      JsObject(ListMap(CR.value.value.writeFields(G.to(a)): _*))
  }

  implicit def hnil[A]: DerivedJsWriter[A, HNil] = (_ => Nil)
  implicit def hcons[A, Key <: Symbol, Value, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LV: Lazy[JsWriter[Value]],
    DR: DerivedJsWriter[A, Remaining]
  ): DerivedJsWriter[A, FieldType[Key, Value] :*: Remaining] = {
    case head :*: tail =>
      val other = DR.writeFields(tail)
      LV.value.write(head) match {
        case JsNull => other
        case value  => (Key.value.name -> value) :: other
      }
  }

  implicit def cnil[A]: DerivedJsWriter[A, CNil] = (_ => Nil)
  implicit def ccons[A, Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    C: JsConfig[A],
    Name: Witness.Aux[Name],
    LI: Lazy[JsWriter[Instance]],
    DR: DerivedJsWriter[A, Remaining]
  ): DerivedJsWriter[A, FieldType[Name, Instance] :+: Remaining] = {
    case Inl(ins) =>
      val typehint = (C.typehint -> JsString(Name.value.name))
      LI.value.write(ins) match {
        case JsObject(fields) => typehint :: fields.toList
        case other            => typehint :: (C.valuehint -> other) :: Nil
      }
    case Inr(rem) => DR.writeFields(rem)
  }

}
