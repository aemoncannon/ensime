// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import java.io.File

import simulacrum._

/** JSON serialization. */
@typeclass trait JsWriter[A] { self =>
  @op("toJson") def write(obj: A): JsValue

  final def contramap[B](f: B => A): JsWriter[B] = b => self.write(f(b))
  def xmap[B](f: A => B, g: B => A): JsWriter[B] = contramap(g)
}
object JsWriter {

  implicit val bigDecimal: JsWriter[BigDecimal] = JsNumber(_)
  implicit val int: JsWriter[Int]               = bigDecimal.contramap(BigDecimal(_))
  implicit val long: JsWriter[Long]             = bigDecimal.contramap(BigDecimal(_))
  implicit val float: JsWriter[Float] =
    bigDecimal.contramap(f => BigDecimal(f.toDouble))
  implicit val double: JsWriter[Double] = bigDecimal.contramap(BigDecimal(_))
  implicit val byte: JsWriter[Byte] =
    bigDecimal.contramap(b => BigDecimal(b.toInt))
  implicit val short: JsWriter[Short] =
    bigDecimal.contramap(s => BigDecimal(s.toInt))
  implicit val bigInt: JsWriter[BigInt]   = bigDecimal.contramap(BigDecimal(_))
  implicit val unit: JsWriter[Unit]       = bigDecimal.contramap(_ => BigDecimal(1))
  implicit val boolean: JsWriter[Boolean] = JsBoolean(_)
  implicit val string: JsWriter[String]   = JsString(_)
  implicit val char: JsWriter[Char]       = string.contramap(_.toString)
  implicit val symbol: JsWriter[Symbol]   = string.contramap(_.name)

  implicit def option[A: JsWriter]: JsWriter[Option[A]] = {
    case Some(a) => JsWriter[A].write(a)
    case None    => JsNull
  }
  implicit def either[A: JsWriter, B: JsWriter]: JsWriter[Either[A, B]] = {
    case Left(a)  => JsWriter[A].write(a)
    case Right(b) => JsWriter[B].write(b)
  }

  implicit val jsValue: JsWriter[JsValue]   = identity
  implicit val jsObject: JsWriter[JsObject] = identity

  implicit def traversable[T[a] <: Traversable[a], A: JsWriter]
    : JsWriter[T[A]] = { ss =>
    val A = JsWriter[A]
    JsArray(ss.map(A.write).toList)
  }
  implicit def dict[V: JsWriter]: JsWriter[Map[String, V]] = { m =>
    val fields = m.map {
      case (k, v) =>
        k -> JsWriter[V].write(v)
    }
    JsObject(fields)
  }

  // legacy format for things with tuples in them...
  // https://github.com/ensime/ensime-server/issues/1557
  implicit def tuple2[A: JsWriter, B: JsWriter]: JsWriter[(A, B)] = {
    case (a, b) => JsArray(JsWriter[A].write(a), JsWriter[B].write(b))
  }

  implicit val file: JsWriter[File] = (f => JsString(f.getPath))

}
