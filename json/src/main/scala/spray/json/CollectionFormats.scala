// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait CollectionFormats {

  implicit def cbf[T[_], A: JsonFormat](
    implicit CBF: CanBuildFrom[Nothing, A, T[A]],
    E: T[A] <:< Traversable[A]
  ): RootJsonFormat[T[A]] = vectorFormat[A].xmap(
    _.to,
    _.toVector
  )

  /**
   * Supplies the JsonFormat for Lists.
   */
  private[this] def vectorFormat[T: JsonFormat] =
    RootJsonFormat.instance[Vector[T]] { vec =>
      JsArray(vec.map(_.toJson))
    } {
      case JsArray(elements) =>
        elements.map(_.convertTo[T])
      case x => deserializationError("Expected List as JsArray, but got " + x)
    }

  /**
   * Supplies the JsonFormat for Maps. The implicitly available JsonFormat for the key type K must
   * always write JsStrings, otherwise a [[spray.json.SerializationException]] will be thrown.
   *
   * We could have type safety by introducing a JsStringFormat.
   */
  implicit def mapFormat[K: JsonFormat, V: JsonFormat] =
    RootJsonFormat.instance[Map[K, V]] { m =>
      JsObject {
        m.map { field =>
          field._1.toJson match {
            case JsString(x) => x -> field._2.toJson
            case x =>
              throw new SerializationException(
                "Map key must be formatted as JsString, not '" + x + "'"
              )
          }
        }
      }
    } {
      case x: JsObject =>
        x.fields.map { field =>
          (JsString(field._1).convertTo[K], field._2.convertTo[V])
        }(collection.breakOut)
      case x => deserializationError("Expected Map as JsObject, but got " + x)
    }
}
