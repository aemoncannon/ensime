// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/** JSON serialization. */
trait JsonWriter[A] { self =>
  def write(obj: A): JsValue

  final def contramap[B](f: B => A): JsonWriter[B] = new JsonWriter[B] {
    override def write(b: B): JsValue = self.write(f(b))
  }
}
object JsonWriter {
  @inline def apply[A](implicit i: JsonWriter[A]): JsonWriter[A] = i
  @inline def instance[A](f: A => JsValue): JsonWriter[A] = new JsonWriter[A] {
    override def write(obj: A) = f(obj)
  }
}

/** JSON deserialization */
trait JsonReader[A] { self =>
  def read(json: JsValue): A

  final def map[B](f: A => B): JsonReader[B] = new JsonReader[B] {
    override def read(v: JsValue): B = f(self.read(v))
  }

  // what the signature should have been to begin with
  final def safe: JsonReader[Either[Exception, A]] =
    new JsonReader[Either[Exception, A]] {
      def read(json: JsValue) =
        try {
          Right(self.read(json))
        } catch {
          case e: Exception => Left(e)
        }
    }

}
object JsonReader {
  @inline def apply[A](implicit i: JsonReader[A]): JsonReader[A] = i
  @inline def instance[A](f: JsValue => A): JsonReader[A] = new JsonReader[A] {
    def read(json: JsValue) = f(json)
  }
}

/** Combined JSON deserialization and serialization */
trait JsonFormat[A] extends JsonReader[A] with JsonWriter[A] { self =>
  def xmap[B](f: A => B, g: B => A): JsonFormat[B] = new JsonFormat[B] {
    override def write(b: B): JsValue = self.write(g(b))
    override def read(s: JsValue): B  = f(self.read(s))
  }
}
object JsonFormat {
  @inline def apply[A](implicit i: JsonFormat[A]): JsonFormat[A] = i
  @inline def instance[A](w: A => JsValue)(r: JsValue => A): JsonFormat[A] =
    new JsonFormat[A] {
      override def write(v: A): JsValue = w(v)
      override def read(v: JsValue): A  = r(v)
    }
}

// remove these horrible things
trait RootJsonReader[A] extends JsonReader[A]
trait RootJsonWriter[A] extends JsonWriter[A]
trait RootJsonFormat[A]
    extends JsonFormat[A]
    with RootJsonReader[A]
    with RootJsonWriter[A] { self =>
  override def xmap[B](f: A => B, g: B => A): RootJsonFormat[B] =
    new RootJsonFormat[B] {
      override def write(b: B): JsValue = self.write(g(b))
      override def read(s: JsValue): B  = f(self.read(s))
    }

}
object RootJsonFormat {
  @inline def apply[A](implicit i: RootJsonFormat[A]): RootJsonFormat[A] = i
  @inline def instance[A](w: A => JsValue)(r: JsValue => A): RootJsonFormat[A] =
    new RootJsonFormat[A] {
      override def write(v: A): JsValue = w(v)
      override def read(v: JsValue): A  = r(v)
    }
}
