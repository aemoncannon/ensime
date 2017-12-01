// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util.UUID

import simulacrum._

/** Provides S-Expression serialization. */
@typeclass trait SexpWriter[A] { self =>
  @op("toSexp") def write(a: A): Sexp

  final def contramap[B](f: B => A): SexpWriter[B]       = b => self.write(f(b))
  final def xmap[B](f: A => B, g: B => A): SexpWriter[B] = contramap(g)
}
object SexpWriter {

  implicit val unit: SexpWriter[Unit] = _ => SexpNil
  implicit val boolean: SexpWriter[Boolean] = {
    case true  => SexpSymbol("t")
    case false => SexpNil
  }
  implicit val char: SexpWriter[Char]     = SexpChar(_)
  implicit val string: SexpWriter[String] = SexpString(_)
  implicit val symbol: SexpWriter[Symbol] = s => SexpSymbol(s.name)
  implicit val long: SexpWriter[Long]     = SexpInteger(_)
  implicit val int: SexpWriter[Int]       = long.contramap(_.toLong)
  implicit val byte: SexpWriter[Byte]     = long.contramap(_.toLong)
  implicit val short: SexpWriter[Short]   = long.contramap(_.toLong)
  implicit val double: SexpWriter[Double] = SexpFloat(_)
  implicit val float: SexpWriter[Float]   = double.contramap(_.toDouble)

  implicit def option[A: SexpWriter]: SexpWriter[Option[A]] = {
    case Some(a) => SexpWriter[A].write(a)
    case None    => SexpNil
  }
  implicit def either[A: SexpWriter, B: SexpWriter]
    : SexpWriter[Either[A, B]] = {
    case Left(a)  => SexpWriter[A].write(a)
    case Right(b) => SexpWriter[B].write(b)
  }
  implicit val uuid: SexpWriter[UUID] = string.contramap(_.toString)
  implicit val uri: SexpWriter[URI]   = string.contramap(_.toASCIIString)
  implicit val file: SexpWriter[File] = string.contramap(_.getPath)
  implicit val path: SexpWriter[Path] = string.contramap(_.toString)

  implicit val sexp: SexpWriter[Sexp] = identity

  implicit def traversable[T[a] <: Traversable[a], A: SexpWriter]
    : SexpWriter[T[A]] = { t =>
    val W = SexpWriter[A]
    SexpList(t.map(W.write)(collection.breakOut): List[Sexp])
  }
  implicit def dict[A: SexpWriter, B: SexpWriter]: SexpWriter[Map[A, B]] = {
    m =>
      val A = SexpWriter[A]
      val B = SexpWriter[B]
      val entries: List[Sexp] = m.map {
        case (a, b) => SexpList(A.write(a), B.write(b))
      }(collection.breakOut)
      SexpList(entries)
  }

  // legacy format for things with tuples in them...
  // https://github.com/ensime/ensime-server/issues/1557
  implicit def tuple2[A: SexpWriter, B: SexpWriter]: SexpWriter[(A, B)] = {
    case (a, b) =>
      SexpData(
        SexpSymbol(":_1") -> SexpWriter[A].write(a),
        SexpSymbol(":_2") -> SexpWriter[B].write(b)
      )
  }

}
