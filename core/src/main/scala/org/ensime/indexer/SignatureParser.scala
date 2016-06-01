package org.ensime.indexer

import org.parboiled2._

import scala.annotation.switch
import scala.util.{ Failure, Success }

object SignatureParser {

  def parseGeneric(desc: String): GenericClass = {
    val parser = new SignatureParser(desc)

    parser.Generic.run() match {
      case Success(sig) => sig
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception(s"Failed to parse generic: $msg")
      case Failure(other) =>
        throw new Exception("Failed to parse generic: ", other)
    }
  }

  val GenericNameCharPredicate = CharPredicate.All -- ":;/ "
}

class SignatureParser(val input: ParserInput) extends ClassParser {

  def Generic: Rule1[GenericClass] = rule {
    run {
      (cursorChar: @switch) match {
        case 'L' => GenericSuper ~ EOI
        case '<' => GenericWithParam ~ EOI
        case _ => MISMATCH
      }
    }
  }

  private def GenericWithParam: Rule1[GenericClass] = rule {
    '<' ~ oneOrMore(GenericSigParam) ~ '>' ~ oneOrMore(GenericClassSig) ~> GenericClass.apply _
  }

  private def GenericSuper: Rule1[GenericClass] = rule {
    oneOrMore(GenericClassSig) ~> (GenericClass(Seq.empty, _: Seq[GenericClassName]))
  }

  // class SomeClass[T <: SomeTrait] will have two : in signature
  private def GenericSigParam: Rule1[GenericParam] = rule {
    GenericName ~ ':' ~ oneOrMore(optional(':') ~ GenericClassSig) ~> GenericParam.apply _
  }

  private def GenericName: Rule1[String] = rule {
    capture(oneOrMore(SignatureParser.GenericNameCharPredicate))
  }

  protected def GenericClassSig: Rule1[GenericClassName] = rule {
    GenericClassSigWithArgs | GenericClassSigWithoutArgs
  }

  protected def GenericClassSigWithArgs: Rule1[GenericClassName] = rule {
    ClassNameSig ~ '<' ~ GenericArgs ~ '>' ~ ';' ~> GenericClassName.apply _
  }

  protected def GenericClassSigWithoutArgs: Rule1[GenericClassName] = rule {
    ClassNameSig ~ ';' ~> (GenericClassName(_: ClassName, Seq.empty))
  }

  protected def PrimitiveClassSig: Rule1[GenericClassName] = rule {
    PrimitiveClass ~> (GenericClassName(_: ClassName, Seq.empty))
  }

  private def GenericArgs: Rule1[Seq[GenericArg]] = rule {
    oneOrMore(ExtendsObject | GenericArgWithSignature)
  }

  private def ExtendsObject: Rule1[GenericArg] = rule {
    ExtendsObjectStar ~> ExtendsObjectGenericArg.apply _
  }

  private def ExtendsObjectStar: Rule0 = rule {
    '*'
  }

  private def GenericArgWithSignature: Rule1[GenericArg] = rule {
    optional(LowerBoundary | UpperBoundary) ~ FieldTypeSignature ~> SpecifiedGenericArg.apply _
  }

  private def LowerBoundary: Rule1[BoundType] = rule {
    LowerB ~> LowerBound.apply _
  }

  private def LowerB: Rule0 = rule {
    '-'
  }

  private def UpperBoundary: Rule1[BoundType] = rule {
    UpperB ~> UpperBound.apply _
  }

  private def UpperB: Rule0 = rule {
    '+'
  }

  private def FieldTypeSignature: Rule1[SignatureType] = rule {
    GenericClassSig | GenericArraySig | TypeVar
  }

  private def GenericArraySig: Rule1[GenericArray] = rule {
    '[' ~ (PrimitiveClassSig | GenericClassSig) ~> GenericArray.apply _
  }

  private def TypeVar: Rule1[GenericVar] = rule {
    'T' ~ capture(oneOrMore(SignatureParser.GenericNameCharPredicate)) ~ ';' ~> GenericVar.apply _
  }

  override val PackageNamePredicate = CharPredicate.All -- "<;/ "
  override val ClassNameCharPredicate = CharPredicate.All -- "<;/ "
}
