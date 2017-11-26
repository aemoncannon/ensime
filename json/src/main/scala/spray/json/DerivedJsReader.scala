// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

import DeserializationException._

trait DerivedJsReader[Base, A] {
  def readObject(t: Map[String, JsValue]): A
}
object DerivedJsReader {

  def gen[A, Repr](
    implicit
    C: JsConfig[A],
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedJsReader[A, Repr]]]
  ): JsReader[A] = {
    case JsObject(m) => G.from(CR.value.value.readObject(m))
    case x           => deserializationError(s"Expected JsObject, got $x")
  }

  implicit def hnil[A]: DerivedJsReader[A, HNil] = (_ => HNil)
  implicit def hcons[A, Key <: Symbol, Value, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LV: Lazy[JsReader[Value]],
    DR: DerivedJsReader[A, Remaining]
  ): DerivedJsReader[A, FieldType[Key, Value] :*: Remaining] = { m =>
    val read = LV.value.read(m.getOrElse(Key.value.name, JsNull))
    field[Key](read) :: DR.readObject(m)
  }

  implicit def cnil[A]: DerivedJsReader[A, CNil] =
    (_ => deserializationError("unknown typehint"))
  implicit def ccons[A, Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    C: JsConfig[A],
    Name: Witness.Aux[Name],
    LI: Lazy[JsReader[Instance]],
    DR: DerivedJsReader[A, Remaining]
  ): DerivedJsReader[A, FieldType[Name, Instance] :+: Remaining] = { obj =>
    val key = Name.value.name
    obj.get(C.typehint) match {
      case Some(JsString(`key`)) =>
        val got = try {
          LI.value.read(JsObject(obj))
        } catch {
          case d: DeserializationException =>
            obj.get(C.valuehint) match {
              case None    => throw d
              case Some(v) => LI.value.read(v)
            }
        }
        Inl(field[Name](got))
      case _ =>
        Inr(DR.readObject(obj))
    }
  }

}
