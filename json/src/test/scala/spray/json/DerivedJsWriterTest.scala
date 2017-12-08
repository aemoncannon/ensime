// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._

class DerivedJsWriterTest extends FlatSpec {
  import JsWriter.ops._
  import examples._

  implicit class Helper[T: JsWriter](t: T) {
    def jsonString: String = CompactPrinter(t.toJson)
  }

  "DerivedJsWriter" should "support anyval" in {
    Optimal("hello").toJson shouldBe JsString("hello")
  }

  it should "support generic products" in {
    Foo("hello").jsonString shouldBe """{"s":"hello"}"""
    Baz.jsonString shouldBe "{}"
    Faz(Some("hello")).jsonString shouldBe """{"o":"hello"}"""
  }

  it should "support generic coproducts" in {
    (Foo("hello"): SimpleTrait).jsonString shouldBe """{"typehint":"Foo","s":"hello"}"""
    (Baz: SimpleTrait).jsonString shouldBe """{"typehint":"Baz"}"""

    (Wibble: AbstractThing).jsonString shouldBe """{"t":"Wibble"}"""
    (Wobble("hello"): AbstractThing).jsonString shouldBe """{"t":"Wobble","id":"hello"}"""

    (Time("goodbye"): NotAnObject).jsonString shouldBe """{"typehint":"Time","value":"goodbye"}"""
    (Money(13): NotAnObject).jsonString shouldBe """{"typehint":"Money","i":13}"""
  }

  it should "support generic recursive ADTs" in {
    val rec = Recursive("hello", Some(Recursive("goodbye")))
    rec.jsonString shouldBe """{"h":"hello","t":{"h":"goodbye"}}"""
  }

}
