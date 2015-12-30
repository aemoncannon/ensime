package org.ensime.core

import org.ensime.fixture._
import org.ensime.api._
import org.scalatest._
import scala.collection.mutable.ListBuffer

class StructureViewBuilderSpec extends WordSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  def original = EnsimeConfigFixture.EmptyTestProject

  "StructureViewBuilder" should {

    def getStructure(
      config: EnsimeConfig,
      cc: RichCompilerControl,
      content: String
    ): List[String] = {

      val result = ListBuffer[String]()

      def collect(parent: Option[String], x: StructureViewMember): Unit = {
        val par = parent.map(_ + ".").getOrElse("")
        x match {
          case StructureViewMember(key, name, _, Nil) =>
            result.append(s"($key)${par}$name")
          case StructureViewMember(key, name, _, xs) =>
            result.append(s"($key)${par}$name")
            xs.foreach(collect(Some(s"${par}$name"), _))
        }
      }

      val file = srcFile(config, "abc.scala", contents(content))
      cc.askLoadedTyped(file)
      cc.askStructure(file).foreach(collect(None, _))
      result.toList
    }

    "show top level classes and objects" in withPresCompiler { (config, cc) =>
      val structure = getStructure(
        config, cc, """
            package com.example
            import org.scalatest._
            class Test {
              def fun(u: Int, v: Int) { u + v }
            }
            object Test {
              def apply(x: String) { new Test(x) }
            }
          """
      )
      assert(structure === List(
        "(class)Test",
        "(def)Test.fun",
        "(object)Test",
        "(def)Test.apply"
      ))
    }

    "show nested members" in withPresCompiler { (config, cc) =>
      val structure = getStructure(
        config, cc, """
            package com.example
            object Test {
              type TestType = Int
              class Nested {
                def fun(u: Int, v: Int) { u + v }
              }
              object Nested {
                def apply(x: String) { new Nested(x) }
              }
            }
          """
      )
      assert(structure === List(
        "(object)Test",
        "(type)Test.TestType",
        "(class)Test.Nested",
        "(def)Test.Nested.fun",
        "(object)Test.Nested",
        "(def)Test.Nested.apply"
      ))
    }

    "skip accessors" in withPresCompiler { (config, cc) =>
      val structure = getStructure(
        config, cc, """
            package com.example
            class Test(val accessor: String)
            class CaseTest(x: String, y: Int)
            object Test {
              class Nested(val accessor: String)
              case class NestedCase(x: String, y:Int)
            }
          """
      )
      assert(structure === List(
        "(class)Test",
        "(class)CaseTest",
        "(object)Test",
        "(class)Test.Nested",
        "(class)Test.NestedCase"
      ))
    }

  }
}
