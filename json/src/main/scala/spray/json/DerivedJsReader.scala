// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

trait DerivedJsReader[Base, A] {
  def readObject(t: Map[String, JsValue]): Either[DeserializationException, A]
}
object DerivedJsReader {

  def gen[A, Repr](
    implicit
    C: JsConfig[A],
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedJsReader[A, Repr]]]
  ): JsReader[A] = {
    case JsObject(m) => CR.value.value.readObject(m).map(G.from)
    case x           => Left(DeserializationException(s"Expected JsObject, got $x"))
  }

  implicit def hnil[A]: DerivedJsReader[A, HNil] = (_ => Right(HNil))
  implicit def hcons[A, Key <: Symbol, Value, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LV: Lazy[JsReader[Value]],
    DR: DerivedJsReader[A, Remaining]
  ): DerivedJsReader[A, FieldType[Key, Value] :*: Remaining] = { m =>
    for {
      head <- LV.value.read(m.getOrElse(Key.value.name, JsNull))
      tail <- DR.readObject(m)
    } yield field[Key](head) :: tail
  }

  implicit def cnil[A]: DerivedJsReader[A, CNil] =
    (_ => Left(DeserializationException("unknown typehint")))
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
        val got = LI.value.read(JsObject(obj)) match {
          case r @ Right(_) => r
          case ex @ Left(_) =>
            obj.get(C.valuehint) match {
              case None    => ex
              case Some(v) => LI.value.read(v)
            }
        }
        got.map(v => Inl(field[Name](v)))
      case _ =>
        DR.readObject(obj).map(Inr.apply)
    }
  }

}
