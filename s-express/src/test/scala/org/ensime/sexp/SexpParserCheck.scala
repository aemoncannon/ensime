// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import org.scalacheck._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SexpParserCheck
    extends SexpSpec
    with GeneratorDrivenPropertyChecks
    with ArbitrarySexp {

  "SexpParser" should "round-trip Sexp <=> String" in {
    forAll { (sexp: Sexp) =>
      val compact = SexpCompactPrinter(sexp)
      //println(compact)
      val pretty = SexpPrettyPrinter(sexp)
      // it might be worthwhile creating a test-only printer that adds
      // superfluous whitespace/comments

      withClue(compact)(SexpParser(compact) shouldBe sexp)
      withClue(pretty)(SexpParser(pretty) shouldBe sexp)
    }
  }
}

trait ArbitrarySexp {

  import org.scalacheck.Arbitrary._
  import org.scalacheck.Gen._

  // avoid stackoverflows with http://stackoverflow.com/questions/19829293

  lazy val genSexpSymbol: Gen[SexpSymbol] =
    alphaStr.filter(_.nonEmpty).map(SexpSymbol)

  lazy val genSexpKey: Gen[SexpSymbol] =
    alphaStr.filter(_.nonEmpty).map { s =>
      SexpSymbol(":" + s)
    }

  lazy val genSexpAtom: Gen[SexpAtom] = oneOf(
    alphaNumChar.map(SexpChar),
    alphaStr.map(SexpString),
    genSexpSymbol,
    arbitrary[Double].map(SexpFloat(_)),
    arbitrary[Long].map(SexpInteger(_)),
    oneOf(SexpNil,
          //SexpFloat(Double.NaN), // can't assert on equality
          SexpFloat(Double.NegativeInfinity),
          SexpFloat(Double.PositiveInfinity))
  )

  def genSexpCons(level: Int): Gen[SexpCons] =
    for {
      car <- genSexpAtom
      cdr <- genSexp(level + 1)
    } yield SexpCons(car, cdr)

  def genSexpList(level: Int): Gen[Sexp] =
    nonEmptyListOf(genSexp(level + 1)).map(SexpList(_))

  def genSexpData(level: Int): Gen[Sexp] =
    mapOfN(2, zip(genSexpKey, genSexp(level + 1))).map { kvs =>
      SexpData(kvs.toList)
    }

  // our parser is soooo slow for deep trees
  def genSexp(level: Int): Gen[Sexp] =
    if (level >= 4) genSexpAtom
    else
      lzy {
        oneOf(
          genSexpAtom,
          genSexpCons(level + 1),
          genSexpList(level + 1),
          genSexpData(level + 1)
        )
      }

  implicit def arbSexp: Arbitrary[Sexp] = Arbitrary(genSexp(0))
}
