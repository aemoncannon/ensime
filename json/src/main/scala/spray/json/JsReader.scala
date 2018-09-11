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
  def read(json: JsValue): Either[DeserializationException, A]

  final def map[B](f: A => B): JsReader[B] =
    (v: JsValue) => self.read(v).map(f)

  final def filter(f: A => Boolean, error: => String): JsReader[A] =
    (v: JsValue) =>
      self.read(v).flatMap { a =>
        if (f(a)) Right(a) else Left(DeserializationException(error))
    }

  def xmap[B](f: A => B, g: B => A): JsReader[B] = map(f)
}
object JsReader {
  object ops {
    implicit class ExtraOps(private val j: JsValue) extends AnyVal {
      def as[A: JsReader]: Either[DeserializationException, A] =
        JsReader[A].read(j)
    }
  }

  implicit val bigDecimal: JsReader[BigDecimal] = {
    case JsNumber(x) => Right(x)
    case JsString(x) =>
      Try(BigDecimal(x)) match {
        case Success(n) => Right(n)
        case Failure(_) =>
          Left(DeserializationException("Expected JsNumber, got " + x))
      }
    case x => Left(DeserializationException("Expected JsNumber, got " + x))
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
    case JsBoolean(x) => Right(x)
    case x            => Left(DeserializationException("Expected JsBoolean, but got " + x))
  }
  implicit val string: JsReader[String] = {
    case JsString(x) => Right(x)
    case x =>
      Left(
        DeserializationException("Expected String as JsString, but got " + x)
      )
  }
  implicit val char: JsReader[Char] = {
    case JsString(x) if x.length == 1 => Right(x.charAt(0))
    case x =>
      Left(DeserializationException("Expected single-character, got " + x))
  }
  implicit val symbol: JsReader[Symbol] = {
    case JsString(x) => Right(Symbol(x))
    case x =>
      Left(
        DeserializationException("Expected Symbol as JsString, but got " + x)
      )
  }

  implicit def option[A: JsReader]: JsReader[Option[A]] = {
    case JsNull => Right(None)
    case a      => JsReader[A].read(a).map(Some(_))
  }
  implicit def either[A: JsReader, B: JsReader]: JsReader[Either[A, B]] = { v =>
    (JsReader[A].read(v), JsReader[B].read(v)) match {
      case (Right(a), Left(_)) => Right(Left(a))
      case (Left(_), Right(b)) => Right(Right(b))
      case (Right(_), Right(_)) =>
        Left(DeserializationException("Ambiguous Either"))
      case (Left(ea), Left(eb)) =>
        Left(DeserializationException(s"Could not read Either: $ea ----- $eb"))
    }
  }

  implicit val jsValue: JsReader[JsValue] = Right.apply
  implicit val jsObject: JsReader[JsObject] = {
    case o: JsObject => Right(o)
    case _           => Left(deserError[JsObject]("expected JsObjec"))
  }

  implicit def cbf[T[_], A: JsReader](
    implicit CBF: CanBuildFrom[Nothing, A, T[A]]
  ): JsReader[T[A]] = {
    case JsArray(values) =>
      values
        .foldLeft[Either[DeserializationException, Seq[A]]](Right(Seq.empty)) {
          case (acc @ Left(_), _) => acc
          case (Right(acc), a)    => JsReader[A].read(a).map(acc :+ _)
        }
        .map(_.to[T])
    case _ =>
      Left(deserError[JsArray]("expected JsArray"))
  }

  implicit def dict[V: JsReader]: JsReader[Map[String, V]] = {
    case x: JsObject =>
      x.fields.foldLeft[Either[DeserializationException, Map[String, V]]](
        Right(Map.empty)
      ) {
        case (acc @ Left(_), _) => acc
        case (Right(acc), (k, v)) =>
          JsReader[V].read(v).map(v => acc + (k -> v))
      }
    case x =>
      Left(DeserializationException("Expected Map as JsObject, but got " + x))
  }

  // legacy format for things with tuples in them...
  // https://github.com/ensime/ensime-server/issues/1557
  implicit def tuple2[A: JsReader, B: JsReader]: JsReader[(A, B)] = {
    case JsArray(Seq(a, b)) =>
      JsReader[A].read(a).flatMap(a => JsReader[B].read(b).map(a -> _))
    case x => Left(DeserializationException("Expected JsArray, got " + x))
  }

  implicit val file: JsReader[File] = {
    case JsString(path) => Right(new File(path))
    case other          => Left(unexpectedJson[File](other))
  }

}
