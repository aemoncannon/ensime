// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import java.io.File

import scala.collection.generic.CanBuildFrom
import scala.util._

import simulacrum._

import DeserializationException._

/** JSON deserialization */
@typeclass(generateAllOps = false) trait JsReader[A] { self =>
  def read(json: JsValue): A

  final def map[B](f: A => B): JsReader[B] = new JsReader[B] {
    override def read(v: JsValue): B = f(self.read(v))
  }
  def xmap[B](f: A => B, g: B => A): JsReader[B] = map(f)
}
object JsReader {
  object ops {
    implicit class ExtraOps(val j: JsValue) extends AnyVal {
      def as[A: JsReader]: A = JsReader[A].read(j)
    }
  }

  implicit val bigDecimal: JsReader[BigDecimal] = {
    case JsNumber(x) => x
    case JsString(x) => BigDecimal(x)
    case x           => deserializationError("Expected JsNumber, got " + x)
  }
  implicit val int: JsReader[Int]       = bigDecimal.map(_.intValue)
  implicit val long: JsReader[Long]     = bigDecimal.map(_.longValue)
  implicit val float: JsReader[Float]   = bigDecimal.map(_.floatValue)
  implicit val double: JsReader[Double] = bigDecimal.map(_.doubleValue)
  implicit val byte: JsReader[Byte]     = bigDecimal.map(_.byteValue)
  implicit val short: JsReader[Short]   = bigDecimal.map(_.shortValue)
  implicit val bigInt: JsReader[BigInt] = bigDecimal.map(_.toBigInt)
  implicit val unit: JsReader[Unit]     = bigDecimal.map(_ => ())
  implicit val boolean: JsReader[Boolean] = {
    case JsBoolean(value) => value
    case x                => deserializationError("Expected JsBoolean, but got " + x)
  }
  implicit val string: JsReader[String] = {
    case JsString(x) => x
    case x           => deserializationError("Expected String as JsString, but got " + x)
  }
  implicit val char: JsReader[Char] = string.map { x =>
    if (x.length == 1) x.charAt(0)
    else deserializationError("Expected single-character, got " + x)
  }
  implicit val symbol: JsReader[Symbol] = {
    case JsString(x) => Symbol(x)
    case x           => deserializationError("Expected Symbol as JsString, but got " + x)
  }

  implicit def option[A: JsReader]: JsReader[Option[A]] = {
    case JsNull => None
    case a      => Some(JsReader[A].read(a))
  }
  implicit def either[A: JsReader, B: JsReader]: JsReader[Either[A, B]] = { v =>
    (Try(JsReader[A].read(v)), Try(JsReader[B].read(v))) match {
      case (Success(a), Failure(_)) => Left(a)
      case (Failure(_), Success(b)) => Right(b)
      case (Success(_), Success(_)) =>
        deserializationError("Ambiguous Either")
      case (Failure(ea), Failure(eb)) =>
        deserializationError(s"Could not read Either: $ea ----- $eb")
    }
  }

  implicit val jsValue: JsReader[JsValue] = identity
  implicit val jsObject: JsReader[JsObject] = {
    case o: JsObject => o
    case _           => deserError[JsObject]("expected JsObjec")
  }

  implicit def cbf[T[_], A: JsReader](
    implicit CBF: CanBuildFrom[Nothing, A, T[A]]
  ): JsReader[T[A]] = {
    case JsArray(values) =>
      val A = JsReader[A]
      values.map(A.read).to[T]
    case _ =>
      deserError[JsArray]("expected JsArray")
  }
  implicit def dict[V: JsReader]: JsReader[Map[String, V]] = {
    case x: JsObject =>
      x.fields.map {
        case (k, v) =>
          k -> JsReader[V].read(v)
      }
    case x => deserializationError("Expected Map as JsObject, but got " + x)
  }

  // legacy format for things with tuples in them...
  // https://github.com/ensime/ensime-server/issues/1557
  implicit def tuple2[A: JsReader, B: JsReader]: JsReader[(A, B)] = {
    case JsArray(Seq(a, b)) => (JsReader[A].read(a), JsReader[B].read(b))
    case x                  => deserializationError("Expected JsArray, got " + x)
  }

  implicit val file: JsReader[File] = {
    case JsString(path) => new File(path)
    case other          => unexpectedJson[File](other)
  }

}
