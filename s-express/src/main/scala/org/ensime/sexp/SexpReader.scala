// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.UUID

import scala.collection.generic.CanBuildFrom

import simulacrum._

/** Provides S-Exp deserialization. */
@typeclass(generateAllOps = false) trait SexpReader[A] { self =>
  def read(s: Sexp): Either[DeserializationException, A]

  final def map[B](f: A => B): SexpReader[B]             = v => self.read(v).map(f)
  final def xmap[B](f: A => B, g: B => A): SexpReader[B] = map(f)
}
object SexpReader {
  object ops {
    implicit class ExtraOps(private val s: Sexp) extends AnyVal {
      def as[A: SexpReader]: Either[DeserializationException, A] =
        SexpReader[A].read(s)
    }
  }

  def instance[A](
    f: PartialFunction[Sexp, Either[DeserializationException, A]]
  ): SexpReader[A] =
    s =>
      f.applyOrElse(s, { _: Sexp =>
        Left(DeserializationException(s))
      })

  implicit val unit: SexpReader[Unit] = instance { case SexpNil => Right(()) }
  implicit val boolean: SexpReader[Boolean] = {
    case SexpNil => Right(false)
    case _       => Right(true) // all non-nil Sexps are technically "true"
  }
  implicit val char: SexpReader[Char] = instance {
    case SexpChar(c) => Right(c)
  }
  implicit val string: SexpReader[String] = instance {
    case SexpString(s) => Right(s)
  }
  implicit val symbol: SexpReader[Symbol] = instance {
    case SexpSymbol(s) => Right(Symbol(s))
  }
  implicit val long: SexpReader[Long] = instance {
    case SexpInteger(i) => Right(i)
  }
  implicit val int: SexpReader[Int]     = long.map(_.toInt)   // can fail
  implicit val byte: SexpReader[Byte]   = long.map(_.toByte)  // can fail
  implicit val short: SexpReader[Short] = long.map(_.toShort) // can fail
  implicit val double: SexpReader[Double] = instance {
    case SexpFloat(d) => Right(d)
  }
  implicit val float: SexpReader[Float] = double.map(_.toFloat) // can fail

  // doesn't roundtrip for A and B with similar representation
  implicit def option[A: SexpReader]: SexpReader[Option[A]] = instance {
    case SexpNil => Right(None)
    case a       => SexpReader[A].read(a).map(Some.apply)
  }
  // doesn't roundtrip for A and B with similar representation
  implicit def either[A: SexpReader, B: SexpReader]
    : SexpReader[Either[A, B]] = { s =>
    SexpReader[A].read(s) match {
      case r @ Left(_)  => SexpReader[B].read(s).map(Right.apply)
      case r @ Right(_) => r.map(Left.apply)
    }
  }
  implicit val uuid: SexpReader[UUID] = string.map(UUID.fromString) // can fail
  implicit val uri: SexpReader[URI]   = string.map(s => new URI(s)) // can fail
  implicit val file: SexpReader[File] = string.map(f => new File(f))
  implicit val path: SexpReader[Path] = string.map(p => Paths.get(p))

  implicit val sexp: SexpReader[Sexp] = Right.apply

  implicit def cbf[T[_], A: SexpReader](
    implicit CBF: CanBuildFrom[Nothing, A, T[A]]
  ): SexpReader[T[A]] = instance {
    case SexpList(values) =>
      values
        .foldLeft[Either[DeserializationException, Seq[A]]](Right(Seq.empty)) {
          case (acc @ Left(_), _) => acc
          case (Right(acc), a)    => SexpReader[A].read(a).map(acc :+ _)
        }
        .map(_.to[T])
  }
  implicit def dict[A: SexpReader, B: SexpReader]: SexpReader[Map[A, B]] =
    instance {
      case SexpList(values) =>
        values.foldLeft[Either[DeserializationException, Map[A, B]]](
          Right(Map.empty)
        ) {
          case (acc @ Left(_), _) => acc
          case (Right(acc), v) =>
            v match {
              case SexpCons(a, SexpCons(b, SexpNil)) =>
                for {
                  aa <- SexpReader[A].read(a)
                  bb <- SexpReader[B].read(b)
                } yield (acc + (aa -> bb))
              case got => Left(DeserializationException(got))
            }
        }
    }

  // legacy format for things with tuples in them...
  // https://github.com/ensime/ensime-server/issues/1557
  implicit def tuple2[A: SexpReader, B: SexpReader]: SexpReader[(A, B)] =
    instance {
      case data @ SexpData(m) =>
        (m.get(SexpSymbol(":_1")), m.get(SexpSymbol(":_2"))) match {
          case (Some(a), Some(b)) =>
            for {
              aa <- SexpReader[A].read(a)
              bb <- SexpReader[B].read(b)
            } yield (aa -> bb)
          case _ => Left(DeserializationException(data))
        }
    }

}
