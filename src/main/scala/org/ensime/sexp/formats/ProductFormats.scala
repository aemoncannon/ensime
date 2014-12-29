package org.ensime.sexp.formats

import akka.event.slf4j.SLF4JLogging
import org.ensime.sexp._
import scala.tools.scalap.scalax.rules.scalasig.ScalaSigParser

/**
 * In Scala 2.10+ we can use Shapeless to automatically derive these
 * product formats, but with Scala 2.9 we incur a lot of boilerplate.
 * Only what is necessary for ENSIME has been implemented.
 */
trait ProductFormats extends SLF4JLogging {

  def tupleFormat2[P1, P2](implicit p1f: SexpFormat[P1], p2f: SexpFormat[P2]) = new SexpFormat[(P1, P2)] {
    def write(t: (P1, P2)) = SexpList(t._1.toSexp, t._2.toSexp)
    def read(s: Sexp): (P1, P2) = s match {
      case SexpList(p1 :: p2 :: Nil) => (p1.convertTo[P1], p2.convertTo[P2])
      case _ => deserializationError(s)
    }
  }

  def productFormat0[T <: Product](cons: () => T) = new SexpFormat[T] {
    def write(t: T) = SexpNil
    def read(s: Sexp): T = cons()
  }

  def productFormat1[P1, T <: Product](
    cons: (P1) => T)(
      implicit p1f: SexpFormat[P1],
      cm: ClassManifest[T]) = new SexpFormat[T] {
    private val List(n1) = extractFieldNames(cm)
    def write(t: T) = SexpData(
      toField[P1](n1, t, 0)
    )
    def read(s: Sexp): T = cons(
      fromField[P1](s, n1)
    )
  }

  def productFormat2[P1, P2, T <: Product](
    cons: (P1, P2) => T)(
      implicit p1f: SexpFormat[P1],
      p2f: SexpFormat[P2],
      cm: ClassManifest[T]) = new SexpFormat[T] {
    private val List(n1, n2) = extractFieldNames(cm)
    def write(t: T) = SexpData(
      toField[P1](n1, t, 0),
      toField[P2](n2, t, 1)
    )
    def read(s: Sexp): T = cons(
      fromField[P1](s, n1),
      fromField[P2](s, n2)
    )
  }

  def productFormat3[P1, P2, P3, T <: Product](
    cons: (P1, P2, P3) => T)(
      implicit p1f: SexpFormat[P1],
      p2f: SexpFormat[P2],
      p3f: SexpFormat[P3],
      cm: ClassManifest[T]) = new SexpFormat[T] {
    private val List(n1, n2, n3) = extractFieldNames(cm)
    def write(t: T) = SexpData(
      toField[P1](n1, t, 0),
      toField[P2](n2, t, 1),
      toField[P3](n3, t, 2)
    )
    def read(s: Sexp): T = cons(
      fromField[P1](s, n1),
      fromField[P2](s, n2),
      fromField[P3](s, n3)
    )
  }

  def productFormat5[P1, P2, P3, P4, P5, T <: Product](
    cons: (P1, P2, P3, P4, P5) => T)(
      implicit p1f: SexpFormat[P1],
      p2f: SexpFormat[P2],
      p3f: SexpFormat[P3],
      p4f: SexpFormat[P4],
      p5f: SexpFormat[P5],
      cm: ClassManifest[T]) = new SexpFormat[T] {
    private val List(n1, n2, n3, n4, n5) = extractFieldNames(cm)
    def write(t: T) = SexpData(
      toField[P1](n1, t, 0),
      toField[P2](n2, t, 1),
      toField[P3](n3, t, 2),
      toField[P4](n4, t, 3),
      toField[P5](n5, t, 4)
    )
    def read(s: Sexp): T = cons(
      fromField[P1](s, n1),
      fromField[P2](s, n2),
      fromField[P3](s, n3),
      fromField[P4](s, n4),
      fromField[P5](s, n5)
    )
  }

  def productFormat10[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, T <: Product](
    cons: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T)(
      implicit p1f: SexpFormat[P1],
      p2f: SexpFormat[P2],
      p3f: SexpFormat[P3],
      p4f: SexpFormat[P4],
      p5f: SexpFormat[P5],
      p6f: SexpFormat[P6],
      p7f: SexpFormat[P7],
      p8f: SexpFormat[P8],
      p9f: SexpFormat[P9],
      p10f: SexpFormat[P10],
      cm: ClassManifest[T]) = new SexpFormat[T] {
    private val List(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) = extractFieldNames(cm)
    def write(t: T) = SexpData(
      toField[P1](n1, t, 0),
      toField[P2](n2, t, 1),
      toField[P3](n3, t, 2),
      toField[P4](n4, t, 3),
      toField[P5](n5, t, 4),
      toField[P6](n6, t, 5),
      toField[P7](n7, t, 6),
      toField[P8](n8, t, 7),
      toField[P9](n9, t, 8),
      toField[P10](n10, t, 9)
    )
    def read(s: Sexp): T = cons(
      fromField[P1](s, n1),
      fromField[P2](s, n2),
      fromField[P3](s, n3),
      fromField[P4](s, n4),
      fromField[P5](s, n5),
      fromField[P6](s, n6),
      fromField[P7](s, n7),
      fromField[P8](s, n8),
      fromField[P9](s, n9),
      fromField[P10](s, n10)
    )
  }

  def productFormat11[P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, T <: Product](
    cons: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T)(
      implicit p1f: SexpFormat[P1],
      p2f: SexpFormat[P2],
      p3f: SexpFormat[P3],
      p4f: SexpFormat[P4],
      p5f: SexpFormat[P5],
      p6f: SexpFormat[P6],
      p7f: SexpFormat[P7],
      p8f: SexpFormat[P8],
      p9f: SexpFormat[P9],
      p10f: SexpFormat[P10],
      p11f: SexpFormat[P11],
      cm: ClassManifest[T]) = new SexpFormat[T] {
    private val List(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11) = extractFieldNames(cm)
    def write(t: T) = SexpData(
      toField[P1](n1, t, 0),
      toField[P2](n2, t, 1),
      toField[P3](n3, t, 2),
      toField[P4](n4, t, 3),
      toField[P5](n5, t, 4),
      toField[P6](n6, t, 5),
      toField[P7](n7, t, 6),
      toField[P8](n8, t, 7),
      toField[P9](n9, t, 8),
      toField[P10](n10, t, 9),
      toField[P11](n11, t, 10)
    )
    def read(s: Sexp): T = cons(
      fromField[P1](s, n1),
      fromField[P2](s, n2),
      fromField[P3](s, n3),
      fromField[P4](s, n4),
      fromField[P5](s, n5),
      fromField[P6](s, n6),
      fromField[P7](s, n7),
      fromField[P8](s, n8),
      fromField[P9](s, n9),
      fromField[P10](s, n10),
      fromField[P11](s, n11)
    )
  }

  // helpers
  private def toField[T](
    key: SexpSymbol,
    p: Product,
    ix: Int)(
      implicit writer: SexpWriter[T]): (SexpSymbol, Sexp) =
    (key, writer.write(p.productElement(ix).asInstanceOf[T]))

  private def fromField[T](value: Sexp, key: SexpSymbol)(implicit reader: SexpReader[T]) = value match {
    case SexpData(map) =>
      map.get(key) match {
        case Some(value) => reader.read(value)
        case None => reader.read(SexpNil)
      }
    case _ => deserializationError(value)
  }

  private def extractFieldNames(classManifest: ClassManifest[_]): List[SexpSymbol] = {
    val clazz = classManifest.erasure
    import collection.breakOut

    ScalaSigParser.parse(clazz) match {
      case Some(ssig) =>
        // doesn't seem to work in the tests...
        ssig.symbols.collect {
          case x if x.isCaseAccessor & x.name.endsWith(" ") =>
            SexpSymbol(":" + (x.name dropRight 1).replace("$minus", "-"))
        }(breakOut)
      case None =>
        log.warn("falling back to reflection for " + clazz)
        try {
          val copyDefaultMethods = clazz.getMethods.
            filter(_.getName.startsWith("copy$default$")).
            sortBy(_.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)

          val fields = clazz.getDeclaredFields.filterNot(_.getName.startsWith("$"))
          if (copyDefaultMethods.length != fields.length)
            sys.error("Case class " + clazz.getName + " declares additional fields")
          if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
            sys.error("Cannot determine field order of case class " + clazz.getName)
          fields.map { n => SexpSymbol(":" + n.getName.replace("$minus", "-")) }.toList
        } catch {
          case ex => throw new RuntimeException(
            "Cannot automatically determine case class field names and order for " + clazz.getName,
            ex
          )
        }
    }
  }
}
