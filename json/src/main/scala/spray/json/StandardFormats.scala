// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/**
 * Provides the JsonFormats for the non-collection standard types.
 */
trait StandardFormats {

  implicit def optionFormat[T: JF]: JF[Option[T]] = new OptionFormat[T]
  // OptionFormat is special cased in FamilyFormats
  class OptionFormat[T: JF] extends JF[Option[T]] {
    def write(option: Option[T]) = option match {
      case Some(x) => x.toJson
      case None    => JsNull
    }
    def read(value: JsValue) = value match {
      case JsNull => None
      case x      => Some(x.convertTo[T])
    }
    // allows reading the JSON as a Some (useful in container formats)
    def readSome(value: JsValue) = Some(value.convertTo[T])
  }

  implicit def eitherFormat[A: JF, B: JF] = new JF[Either[A, B]] {
    private[this] val safeA = JsonReader[A].safe
    private[this] val safeB = JsonReader[B].safe

    def write(either: Either[A, B]) = either match {
      case Right(a) => a.toJson
      case Left(b)  => b.toJson
    }
    def read(value: JsValue) =
      (safeA.read(value), safeB.read(value)) match {
        case (Right(a), _: Left[_, _]) => Left(a)
        case (_: Left[_, _], Right(b)) => Right(b)
        case (_: Right[_, _], _: Right[_, _]) =>
          deserializationError(
            "Ambiguous Either value: can be read as both, Left and Right, values"
          )
        case (Left(ea), Left(eb)) =>
          deserializationError(
            "Could not read Either value:\n" + ea + "---------- and ----------\n" + eb
          )
      }
  }

}
