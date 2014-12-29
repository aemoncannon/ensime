package org.ensime.sexp.formats

import org.ensime.sexp._
import shapeless._

import SexpFormatUtils._

class FamilyFormatsSpec extends FormatSpec with FamilyFormats {

  case object Bloo

  describe("FamilyFormats") {
    // it("should support case objects") {
    //   assertFormat(Bloo, SexpSymbol(Bloo.getClass.getName))
    // }

    it("should support an example AST") {
      import DefaultSexpProtocol._
      import ExampleAst._

      /////////////////// START OF BOILERPLATE /////////////////

      implicit val DatabaseFieldF = productFormat1(DatabaseField)
      implicit val PreferF = productFormat1(Prefer)
      implicit val FieldTermF = productFormat3(FieldTerm)
      implicit val BoundedTermF = productFormat5(BoundedTerm)
      implicit val UnparsedF = productFormat1(Unparsed)
      implicit val IgnoredF = productFormat1(Ignored)
      implicit val UnclearF = productFormat1(Unclear)
      implicit val InTermF = productFormat3(InTerm)
      implicit val LikeF = productFormat1(Like)
      implicit val LikeTermF = productFormat2(LikeTerm)
      implicit val QualifierTokenF = productFormat2(QualifierToken)

      implicit def AndConditionF = productFormat3(AndCondition)
      implicit def OrConditionF = productFormat3(OrCondition)

      implicit def TokenTreeFormat: SexpFormat[TokenTree] = new TraitFormat[TokenTree] {
        // get a performance improvement by creating as many implicit vals
        // for TypeHint[T] as possible, e.g.
        // implicit val FieldTermTH = typehint[FieldTerm]
        // implicit val BoundedTermTH = typehint[BoundedTerm]
        // implicit val UnparsedTH = typehint[Unparsed]
        // implicit val IgnoredTH = typehint[Ignored]
        // implicit val UnclearTH = typehint[Unclear]
        // implicit val InTermTH = typehint[InTerm]
        // implicit val LikeTermTH = typehint[LikeTerm]
        // implicit val OrConditionTH = typehint[OrCondition]
        // implicit val AndConditionTH = typehint[AndCondition]
        // implicit val PreferTokenTH = typehint[PreferToken]
        // implicit val QualifierTokenTH = typehint[QualifierToken]

        def write(obj: TokenTree): Sexp = obj match {
          case f: FieldTerm => wrap(f)
          case b: BoundedTerm => wrap(b)
          case u: Unparsed => wrap(u)
          case i: Ignored => wrap(i)
          case u: Unclear => wrap(u)
          case i: InTerm => wrap(i)
          case like: LikeTerm => wrap(like)
          case a: AndCondition => wrap(a)(typehint[AndCondition], AndConditionF)
          case o: OrCondition => wrap(o)(typehint[OrCondition], OrConditionF)
          case prefer: PreferToken => throw new UnsupportedOperationException
          case q: QualifierToken => wrap(q)
        }

        def read(hint: SexpSymbol, value: Sexp): TokenTree = hint match {
          case s if s == implicitly[TypeHint[FieldTerm]].hint => value.convertTo[FieldTerm]
          case s if s == implicitly[TypeHint[BoundedTerm]].hint => value.convertTo[BoundedTerm]
          case s if s == implicitly[TypeHint[Unparsed]].hint => value.convertTo[Unparsed]
          case s if s == implicitly[TypeHint[Ignored]].hint => value.convertTo[Ignored]
          case s if s == implicitly[TypeHint[Unclear]].hint => value.convertTo[Unclear]
          case s if s == implicitly[TypeHint[InTerm]].hint => value.convertTo[InTerm]
          case s if s == implicitly[TypeHint[LikeTerm]].hint => value.convertTo[LikeTerm]
          case s if s == implicitly[TypeHint[AndCondition]].hint => value.convertTo[AndCondition]
          case s if s == implicitly[TypeHint[OrCondition]].hint => value.convertTo[OrCondition]
          case s if s == implicitly[TypeHint[QualifierToken]].hint => value.convertTo[QualifierToken]
          // SAD FACE --- compiler doesn't catch typos on matches or missing impls
          case _ => deserializationError(hint)
        }
      }

      /////////////////// END OF BOILERPLATE /////////////////

      val fieldTerm = FieldTerm("thing is ten", DatabaseField("THING"), "10")
      val expectField = SexpData(
        SexpSymbol(":text") -> SexpString("thing is ten"),
        SexpSymbol(":field") -> SexpData(
          SexpSymbol(":column") -> SexpString("THING")),
        SexpSymbol(":value") -> SexpString("10"))

      // confirm that the wrapper is picked up for a specific case class
      assertFormat(fieldTerm, expectField)

      val expectFieldTree = SexpData(SexpSymbol(":FieldTerm") -> expectField)

      // confirm that the trait level formatter works
      assertFormat(fieldTerm: TokenTree, expectFieldTree)

      // confirm recursive works
      val and = AndCondition(fieldTerm, fieldTerm, "wibble")
      val expectAnd = SexpData(
        SexpSymbol(":left") -> expectFieldTree,
        SexpSymbol(":right") -> expectFieldTree,
        SexpSymbol(":text") -> SexpString("wibble")
      )
      assertFormat(and, expectAnd)

      val expectAndTree = SexpData(SexpSymbol(":AndCondition") -> expectAnd)

      // and that the recursive type works as a trait
      assertFormat(and: TokenTree, expectAndTree)
    }
  }
}
