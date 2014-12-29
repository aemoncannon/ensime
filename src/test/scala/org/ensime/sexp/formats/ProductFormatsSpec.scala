package org.ensime.sexp.formats

import org.ensime.sexp._

import shapeless._

class ProductFormatsSpec extends FormatSpec
    with BasicFormats with StandardFormats with ProductFormats {

  case class Foo(i: Int, s: String)
  case class Bar(foo: Foo)
  case class Baz()
  case class Wibble(thing: String, thong: Int, bling: Option[String])

  describe("ProductFormats case classes") {
    val foo = Foo(13, "foo")
    val fooexpect = SexpData(
      SexpSymbol(":i") -> SexpNumber(13),
      SexpSymbol(":s") -> SexpString("foo")
    )

    it("should support primitive types") {
      implicit val FooFormat = productFormat2(Foo)

      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
    }

    it("should support nested case classes") {
      val bar = Bar(foo)
      val expect = SexpData(
        SexpSymbol(":foo") -> fooexpect
      )

      implicit val FooFormat = productFormat2(Foo)
      implicit val BarFormat = productFormat1(Bar)

      assertFormat(bar, expect)
    }

    it("should support zero content case classes") {
      implicit val BazFormat = productFormat0(Baz)

      assertFormat(Baz(), SexpNil)
    }

    it("should support missing fields as SexpNil / None") {
      implicit val WibbleFormat = productFormat3(Wibble)

      val wibble = Wibble("wibble", 13, Some("fork"))

      assertFormat(wibble, SexpData(
        SexpSymbol(":thing") -> SexpString("wibble"),
        SexpSymbol(":thong") -> SexpNumber(13),
        SexpSymbol(":bling") -> SexpList(SexpString("fork"))))

      val wobble = Wibble("wibble", 13, None)

      // write out None as SexpNil
      assertFormat(wobble, SexpData(
        SexpSymbol(":thing") -> SexpString("wibble"),
        SexpSymbol(":thong") -> SexpNumber(13),
        SexpSymbol(":bling") -> SexpNil))

      // but tolerate missing entries
      assert(SexpData(
        SexpSymbol(":thing") -> SexpString("wibble"),
        SexpSymbol(":thong") -> SexpNumber(13)
      ).convertTo[Wibble] === wobble)
    }
  }

  describe("ProductFormat tuples") {
    val foo = (13, "foo")
    val fooexpect = SexpList(SexpNumber(13), SexpString("foo"))

    implicit val TupleFooFormat = tupleFormat2[Int, String]

    it("should support primitive types") {
      assertFormat(foo, fooexpect)
    }
  }
}
