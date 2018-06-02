// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

class BasicFormatsSpec extends FormatSpec {

  "BasicFormats" should "support Int" in {
    assertFormat(13, SexpInteger(13))
    assertFormat(-1, SexpInteger(-1))
    assertFormat(0, SexpInteger(0))
    assertFormat(Int.MaxValue, SexpInteger(Int.MaxValue))
    assertFormat(Int.MinValue, SexpInteger(Int.MinValue))
  }

  it should "support Long" in {
    assertFormat(13L, SexpInteger(13L))
    assertFormat(-1L, SexpInteger(-1L))
    assertFormat(0L, SexpInteger(0L))
    assertFormat(Long.MaxValue, SexpInteger(Long.MaxValue))
    assertFormat(Long.MinValue, SexpInteger(Long.MinValue))
  }

  it should "support Float" in {
    assertFormat(13.0f, SexpFloat(13.0f))
    assertFormat(-1.0f, SexpFloat(-1.0f))
    assertFormat(0.0f, SexpFloat(0.0f))
    assertFormat(Float.MaxValue, SexpFloat(Float.MaxValue))
    //assertFormat(Float.MinValue, SexpFloat(Float.MinValue)) // implicit widening?
    assertFormat(Float.NegativeInfinity, SexpFloat(Float.NegativeInfinity))
    assertFormat(Float.PositiveInfinity, SexpFloat(Float.PositiveInfinity))

    // remember NaN != NaN
    SexpWriter[Float].write(Float.NaN) should matchPattern {
      case SexpFloat(f) if f.isNaN =>
    }
    SexpReader[Float].read(SexpFloat(Double.NaN)).map(_.isNaN) shouldBe Right(
      true
    )
  }

  it should "support Double" in {
    assertFormat(13.0d, SexpFloat(13.0d))
    assertFormat(-1.0d, SexpFloat(-1.0d))
    assertFormat(0.0d, SexpFloat(0.0d))
    assertFormat(Double.MaxValue, SexpFloat(Double.MaxValue))
    assertFormat(Double.MinValue, SexpFloat(Double.MinValue))
    assertFormat(Double.NegativeInfinity, SexpFloat(Double.NegativeInfinity))
    assertFormat(Double.PositiveInfinity, SexpFloat(Double.PositiveInfinity))

    // remember NaN != NaN
    SexpWriter[Double].write(Double.NaN) should matchPattern {
      case SexpFloat(f) if f.isNaN =>
    }
    SexpReader[Double].read(SexpFloat(Double.NaN)).map(_.isNaN) shouldBe Right(
      true
    )
  }

  it should "support Boolean" in {
    assertFormat(true, SexpSymbol("t"))
    assertFormat(false, SexpNil)
  }

  it should "support Char" in {
    assertFormat('t', SexpChar('t'))
  }

  it should "support Unit" in {
    assertFormat((), SexpNil)
  }

  it should "support Symbol" in {
    assertFormat('blah, SexpSymbol("blah"))
  }
}
