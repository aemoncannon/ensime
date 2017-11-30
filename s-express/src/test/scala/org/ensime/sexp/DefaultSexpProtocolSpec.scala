// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import org.ensime.sexp._

// these are a sort of aggregated test, investigating behaviours of
// interactions between the different formats.
class DefaultSexpProtocolSpec extends FormatSpec {

  "DefaultSexpProtocol" should "support String as SexpString, not via IsTraversableLike" in {
    assertFormat("hello", SexpString("hello"))
  }

  it should "round-trip Option[List[T]]" in {
    val none: Option[List[String]]  = None
    val empty: Option[List[String]] = Some(Nil)
    val list: Option[List[String]]  = Some(List("boo"))

    //assertFormat(empty, SexpList(SexpNil)) // lossy
    assertFormat(none, SexpNil)
    //assertFormat(list, SexpList(SexpList(SexpString("boo"))))
    assertFormat(list, SexpList(SexpString("boo")))

    SexpReader[Option[List[String]]].read(SexpNil) shouldBe None
    SexpReader[List[Option[String]]].read(SexpNil) shouldBe Nil
  }

}
