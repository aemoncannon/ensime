// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._

class DerivedSexpWriterTest extends FlatSpec {
  import SexpWriter.ops._
  import examples._

  implicit class Helper[T: SexpWriter](t: T) {
    def sexpString: String = SexpCompactPrinter(t.toSexp)
  }

  "DerivedSexpWriter" should "support anyval" in {
    Optimal("hello").toSexp shouldBe SexpString("hello")
  }

  it should "support generic products" in {
    Foo("hello").sexpString shouldBe """(:s "hello")"""
    Baz.sexpString shouldBe "nil"
    Faz(Some("hello")).sexpString shouldBe """(:o "hello")"""
  }

  it should "support generic coproducts" in {
    (Foo("hello"): SimpleTrait).sexpString shouldBe """(:foo (:s "hello"))"""
    (Baz: SimpleTrait).sexpString shouldBe """:baz"""

    (Wibble: AbstractThing).sexpString shouldBe """:WIBBLE"""
    (Wobble("hello"): AbstractThing).sexpString shouldBe """(:WOBBLE (:ID "hello"))"""
  }

  it should "support generic recursive ADTs" in {
    val rec = Recursive("hello", Some(Recursive("goodbye")))
    rec.sexpString shouldBe """(:h "hello" :t (:h "goodbye"))"""
  }

}
