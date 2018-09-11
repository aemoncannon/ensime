// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._

class DerivedJsReaderTest extends FlatSpec {
  import JsReader.ops._
  import examples._

  implicit class Helper(s: String) {
    def parseAs[A: JsReader]: Either[DeserializationException, A] =
      JsParser(s).as[A]
  }

  "DerivedJsWriter" should "support anyval" in {
    JsString("hello").as[Optimal] shouldBe Right(Optimal("hello"))
  }

  it should "support generic products" in {
    """{"s":"hello"}""".parseAs[Foo] shouldBe Right(Foo("hello"))
    """{}""".parseAs[Baz.type] shouldBe Right(Baz)
    """{"o":"hello"}""".parseAs[Faz] shouldBe Right(Faz(Some("hello")))
  }

  it should "support generic coproducts" in {
    """{"typehint":"Foo","s":"hello"}""".parseAs[SimpleTrait] shouldBe Right(
      Foo(
        "hello"
      )
    )
    """{"typehint":"Baz"}""".parseAs[SimpleTrait] shouldBe Right(Baz)

    """{"t":"Wibble"}""".parseAs[AbstractThing] shouldBe Right(Wibble)
    """{"t":"Wobble","id":"hello"}""".parseAs[AbstractThing] shouldBe Right(
      Wobble(
        "hello"
      )
    )

    """{"typehint":"Time","value":"goodbye"}"""
      .parseAs[NotAnObject] shouldBe Right(Time("goodbye"))
    """{"typehint":"Money","i":13}""".parseAs[NotAnObject] shouldBe Right(
      Money(13)
    )
  }

  it should "support generic recursive ADTs" in {
    """{"h":"hello","t":{"h":"goodbye"}}""".parseAs[Recursive] shouldBe
      Right(Recursive("hello", Some(Recursive("goodbye"))))
  }

}
