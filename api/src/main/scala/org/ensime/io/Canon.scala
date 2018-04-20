// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.io

import java.io._
import java.nio.file._

import simulacrum._

import scalaz._
import scalaz.ioeffect.IO
import Scalaz._

/**
 * Transforms values to canonicalised values, eliminating the confusion at API
 * boundaries caused by symbolic links. Most types have a trivial implementation
 * but those that do not must perform IO.
 */
@typeclass trait Canon[A] { self =>
  def canon(a: A): IO[A]

  final def xmap[B](f: A => B, g: B => A): Canon[B] =
    b => self.canon(g(b)).map(f)
}
object Canon extends LowPriorityCanon {

  implicit val file: Canon[File] =
    f => IO.sync(f.getCanonicalFile).catchAll(_ => IO.sync(f.getAbsoluteFile))

  implicit val path: Canon[Path] =
    p =>
      IO.sync {
        val norm = p.normalize()
        val target =
          if (Files.isSymbolicLink(norm)) Files.readSymbolicLink(norm)
          else norm
        try target.toRealPath()
        catch {
          case e: IOException => target
        }
    }

  implicit val string: Canon[String]   = IO.now(_)
  implicit val symbol: Canon[Symbol]   = IO.now(_)
  implicit val boolean: Canon[Boolean] = IO.now(_)
  implicit val int: Canon[Int]         = IO.now(_)
  implicit val long: Canon[Long]       = IO.now(_)
}

trait LowPriorityCanon {
  implicit def bitraverse[F[_, _]: Bitraverse, A: Canon, B: Canon]
    : Canon[F[A, B]] =
    _.bitraverse(Canon[A].canon, Canon[B].canon)

  implicit def traverse[F[_]: Traverse, A: Canon]: Canon[F[A]] =
    _.traverse(Canon[A].canon)
}
