package org.ensime.core

import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import DeclaredAs.{ Nil => _, _ }

class TypeToScalaNameSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.EmptyTestProject

  it should "calculate the TypeInfo at point" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "class Thing {",
      "    val in@int@t: Int = 13",
      "    def met@method1@hod1(i: Int): String = i.toString",
      "    val ar@arrow1@row1: Int => String = (i: Int) => met@call1@hod1(i)",
      "    def met@method2@hod2(i: Int, j: Long): String = i.toString",
      "    val arrow2: Int => Long => String = (i: Int, j: Long) => met@call2@hod2(i, j)",
      "}"
    ) { (p, label, cc) =>
        withClue(label) {
          cc.askTypeInfoAt(p).getOrElse { fail } shouldBe {
            label match {
              case "int" =>
                BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None)
              case "method1" | "method2" =>
                // the return type
                BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None)
              case "call1" =>
                ArrowTypeInfo(
                  // intentionally no surrounding brackets for 1 parameter
                  "Int => String",
                  "scala.Int => java.lang.String",
                  BasicTypeInfo(
                    "String",
                    Class,
                    "java.lang.String",
                    Nil, Nil, None
                  ),
                  List(ParamSectionInfo(List(("i", BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None))), false))
                )
              case "arrow1" =>
                // this is a canary, we'd prefer it to return an ArrowTypeInfo
                BasicTypeInfo(
                  "Function1[Int, String]",
                  Trait,
                  "scala.Function1[scala.Int, java.lang.String]",
                  List(
                    BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None),
                    BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None)
                  ),
                  Nil, None
                )
              case "call2" =>
                ArrowTypeInfo(
                  "(Int, Long) => String",
                  "(scala.Int, scala.Long) => java.lang.String",
                  BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None),
                  List(ParamSectionInfo(List(
                    ("i", BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None)),
                    ("j", BasicTypeInfo("Long", Class, "scala.Long", Nil, Nil, None))
                  ), false))
                )

            }
          }
        }
      }
  }

}
