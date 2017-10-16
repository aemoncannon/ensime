// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package org.ensime.sexp

/** Provides S-Exp serialization. */
trait SexpWriter[A] { self =>
  def write(a: A): Sexp

  final def contramap[B](f: B => A): SexpWriter[B] = new SexpWriter[B] {
    override def write(b: B): Sexp = self.write(f(b))
  }
}
object SexpWriter {
  @inline def apply[A](implicit i: SexpWriter[A]): SexpWriter[A] = i
  @inline def instance[A](f: A => Sexp): SexpWriter[A] = new SexpWriter[A] {
    def write(obj: A) = f(obj)
  }
}

/** Provides S-Exp deserialization. */
trait SexpReader[A] { self =>
  def read(s: Sexp): A

  final def map[B](f: A => B): SexpReader[B] = new SexpReader[B] {
    override def read(v: Sexp): B = f(self.read(v))
  }
}
object SexpReader {
  @inline def apply[A](implicit i: SexpReader[A]): SexpReader[A] = i
  @inline def instance[A](f: Sexp => A): SexpReader[A] = new SexpReader[A] {
    override def read(v: Sexp): A = f(v)
  }
}

/** Provides S-Exp deserialization and serialization. */
trait SexpFormat[A] extends SexpReader[A] with SexpWriter[A] { self =>
  final def xmap[B](f: A => B, g: B => A): SexpFormat[B] = new SexpFormat[B] {
    override def write(b: B): Sexp = self.write(g(b))
    override def read(s: Sexp): B  = f(self.read(s))
  }
}
object SexpFormat {
  @inline def apply[A](implicit i: SexpFormat[A]): SexpFormat[A] = i
  @inline def instance[A](writer: A => Sexp)(reader: Sexp => A): SexpFormat[A] =
    new SexpFormat[A] {
      override def write(a: A): Sexp = writer(a)
      override def read(s: Sexp): A  = reader(s)
    }
}
