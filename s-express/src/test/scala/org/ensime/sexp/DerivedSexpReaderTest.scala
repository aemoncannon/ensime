// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._

class DerivedSexpReaderTest extends FlatSpec {
  import SexpReader.ops._
  import examples._

  implicit class Helper(s: String) {
    def parseAs[A: SexpReader]: A = SexpParser(s).as[A]
  }

  "DerivedSexpWriter" should "support anyval" in {
    SexpString("hello").as[Optimal] shouldBe Optimal("hello")
  }

  it should "support generic products" in {
    """(:s "hello")""".parseAs[Foo] shouldBe Foo("hello")
    "nil".parseAs[Baz.type] shouldBe Baz
    """(:o "hello")""".parseAs[Faz] shouldBe Faz(Some("hello"))
  }

  it should "support generic coproducts" in {
    """(:foo (:s "hello"))""".parseAs[SimpleTrait] shouldBe Foo("hello")
    """:baz""".parseAs[SimpleTrait] shouldBe Baz

    """:WIBBLE""".parseAs[AbstractThing] shouldBe Wibble
    """(:WOBBLE (:ID "hello"))""".parseAs[AbstractThing] shouldBe Wobble(
      "hello"
    )

  }

  it should "support generic recursive ADTs" in {
    """(:h "hello" :t (:h "goodbye"))""".parseAs[Recursive] shouldBe
      Recursive("hello", Some(Recursive("goodbye")))
  }

}
