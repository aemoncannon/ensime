package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.scalatest.FunSpec
import org.scalatest.Matchers
import DescriptorParser.{ parse, parseType }
import ClassName._

class DescriptorParserSpec extends FunSpec with Matchers with SLF4JLogging {

  private val S = ClassName(PackageName(List("java", "lang")), "String")
  private val A = ArrayDescriptor
  private val D = Descriptor
  private val I = PrimitiveInt
  private val V = PrimitiveVoid
  private val Z = PrimitiveBoolean
  private val root = PackageName(Nil)

  describe("DescriptorParser") {
    it("should parse descriptors without parameters") {
      assert(parse("()V") === D(Nil, PrimitiveVoid))
      assert(parse("()Ljava/lang/String;") === D(Nil, S))
      assert(parse("()[Ljava/lang/String;") === D(Nil, A(S)))
      assert(parse("()[[Ljava/lang/String;") === D(Nil, A(A(S))))
      assert(parse("()[[[Ljava/lang/String;") === D(Nil, A(A(A(S)))))
    }

    it("should handle multiple object parameters") {
      assert(parse("(I[IILjava/lang/String;Z)V") === D(List(I, A(I), I, S, Z), V))
    }

    it("should be invertible") {
      def invert(desc: String) =
        assert(parse(desc).descriptorString === desc)

      invert("(I[IILjava/lang/String;Z)V")
    }
  }

  describe("DescriptorParser's JVM internal mode") {
    it("should handle examples") {
      assert(parseType("Ljava/lang/String;") === S)
      assert(parseType("[Ljava/lang/String;") === A(S))
      assert(parseType("[[Ljava/lang/String;") === A(A(S)))
      assert(parseType("V") === V)
      assert(parseType("LMyAnnotation;") === ClassName(root, "MyAnnotation"))
    }

    it("should be invertible") {
      def invert(desc: String) =
        assert(parseType(desc).internalString === desc)

      invert("Ljava/lang/String;")
      invert("[[Ljava/lang/String;")
    }
  }

}
