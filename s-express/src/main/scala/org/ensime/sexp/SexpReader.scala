// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.UUID

import scala.collection.generic.CanBuildFrom
import scala.util.Try

import simulacrum._

/** Provides S-Exp deserialization. */
@typeclass(generateAllOps = false) trait SexpReader[A] { self =>
  def read(s: Sexp): A

  final def map[B](f: A => B): SexpReader[B]             = v => f(self.read(v))
  final def xmap[B](f: A => B, g: B => A): SexpReader[B] = map(f)
}
object SexpReader {
  object ops {
    implicit class ExtraOps(val s: Sexp) extends AnyVal {
      def as[A: SexpReader]: A = SexpReader[A].read(s)
    }
  }

  def instance[A](f: PartialFunction[Sexp, A]): SexpReader[A] =
    s =>
      f.applyOrElse(s, { _: Sexp =>
        throw new DeserializationException(s)
      })

  implicit val unit: SexpReader[Unit] = instance { case SexpNil => () }
  implicit val boolean: SexpReader[Boolean] = {
    case SexpNil => false
    case _       => true // all non-nil Sexps are technically "true"
  }
  implicit val char: SexpReader[Char]     = instance { case SexpChar(c)   => c }
  implicit val string: SexpReader[String] = instance { case SexpString(s) => s }
  implicit val symbol: SexpReader[Symbol] = instance {
    case SexpSymbol(s) => Symbol(s)
  }
  implicit val long: SexpReader[Long]     = instance { case SexpInteger(i) => i }
  implicit val int: SexpReader[Int]       = long.map(_.toInt) // can fail
  implicit val byte: SexpReader[Byte]     = long.map(_.toByte) // can fail
  implicit val short: SexpReader[Short]   = long.map(_.toShort) // can fail
  implicit val double: SexpReader[Double] = instance { case SexpFloat(d) => d }
  implicit val float: SexpReader[Float]   = double.map(_.toFloat) // can fail

  // doesn't roundtrip for A and B with similar representation
  implicit def option[A: SexpReader]: SexpReader[Option[A]] = instance {
    case SexpNil => None
    case a       => Some(SexpReader[A].read(a))
  }
  // doesn't roundtrip for A and B with similar representation
  implicit def either[A: SexpReader, B: SexpReader]
    : SexpReader[Either[A, B]] = { s =>
    Try(Left(SexpReader[A].read(s))).toOption getOrElse Right(
      SexpReader[B].read(s)
    )
  }
  implicit val uuid: SexpReader[UUID] = string.map(UUID.fromString) // can fail
  implicit val uri: SexpReader[URI]   = string.map(s => new URI(s)) // can fail
  implicit val file: SexpReader[File] = string.map(f => new File(f))
  implicit val path: SexpReader[Path] = string.map(p => Paths.get(p))

  implicit val sexp: SexpReader[Sexp] = identity

  implicit def cbf[T[_], A: SexpReader](
    implicit CBF: CanBuildFrom[Nothing, A, T[A]]
  ): SexpReader[T[A]] = instance {
    case SexpList(values) =>
      val A = SexpReader[A]
      values.map(A.read).to[T]
  }
  implicit def dict[A: SexpReader, B: SexpReader]: SexpReader[Map[A, B]] = {
    case SexpList(values) =>
      val A = SexpReader[A]
      val B = SexpReader[B]
      values.map {
        case SexpCons(a, SexpCons(b, SexpNil)) => A.read(a) -> B.read(b)
        case got                               => throw new DeserializationException(got)
      }(collection.breakOut)
  }

  // legacy format for things with tuples in them...
  // https://github.com/ensime/ensime-server/issues/1557
  implicit def tuple2[A: SexpReader, B: SexpReader]: SexpReader[(A, B)] =
    instance {
      case data @ SexpData(m) =>
        (m.get(SexpSymbol(":_1")), m.get(SexpSymbol(":_2"))) match {
          case (Some(a), Some(b)) =>
            (SexpReader[A].read(a), SexpReader[B].read(b))
          case _ => throw new DeserializationException(data)
        }
    }

}
