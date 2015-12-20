package org.ensime.core

import java.io.File

import akka.event.slf4j.SLF4JLogging
import org.ensime.fixture._
import org.ensime.indexer.DescriptorParser
import org.ensime.indexer._
import org.scalatest._

import scala.collection.immutable.Queue
import scala.concurrent.Await
import scala.reflect.internal.util.{ BatchSourceFile, OffsetPosition }
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter
import scala.util.Properties

// core/it:test-only *Fqn*
class SymbolToFqnSpec extends FlatSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture
    with SLF4JLogging {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.DocsTestProject

  "SymbolToFqn" should "calculate the FQNs from Symbols.Symbol" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      // TODO: share with DocFindingSpec
      // TODO: arrays in return
      // TODO: multi-dimensional arrays
      // TODO: a curried method
      // TODO: tests of package objects / members
      // TODO: nested objects
      // TODO: utf and other special characters in class names
      // TODO: Any / AnyVal / AnyRef as a method parameter
      // TODO: fields
      "package com.example",
      "import com.google.common.io.Files",
      "import com.google.common.base.Charsets",
      "import java.nio.channels.FileChannel._",
      "import java.io.File",
      "class Thing {",
      "  def main(): Unit = {",
      "    val o = Some(1)",
      "    val nums = o.m@0@ap(_ + 2)",
      "    val b:Boo@0.5@lean = false",
      "    nums.isDe@1@fined",
      "    val nums2 = o.flat@2@Map {i:Int => Some(i + 1)}",
      "    val x = Some(Some(1)).fla@3@tten",
      "    val y = Some(1).fo@4@ld(0) { ea => ea + 2 }",
      "    val z = Some(1).mkS@5@tring(\".\", \".\", \".\")",
      "    val zz = Some(1).mkS@6@tring",
      "    val zzz = Some(1).mkS@7@tring(\".\")",
      "    val q = Some(1).getOr@8@Else(2)",
      "    val r = Some(1).gro@9@uped(2)",
      "    val xx = List.emp@10@ty",
      "    val f = new Fi@10.5@le(\".\")",
      "    Files.mo@11@ve(f, f)",
      "    Files.asByte@12@Source(f)",
      "    Files.m@13@ap(f, MapMode.PRIVATE)",
      "    Files.m@14@ap(f, MapMode.PRIVATE, 5)",
      "    val a = Arr@14.5@ay[Byte]()",
      "    Files.wri@15@te(a, f)",
      "    val aa = So@16@me(4)",
      /*
       22:40:39.129 WARN   o.e.core.RichPresentationCompiler - No definition found. Please report to https://github.com/ensime/ensime-server/issues/492 with description of what did you expected. symbolAt for class scala.reflect.internal.Trees$Literal: "abcdefg"
       */
      //"    val sss = \"abcd@17@efg\"",
      /*
       22:41:22.860 WARN   o.e.core.RichPresentationCompiler - No definition found. Please report to https://github.com/ensime/ensime-server/issues/492 with description of what did you expected. symbolAt for class scala.reflect.internal.Trees$Literal: 123456
       */
      //"    val ii = 123@18@456",
      "    val fo@20@x = new File(\".\")",
      // "    val c = classOf[File].isInst@21@ance(fox)",
      // "    scala.Predef.DummyIm@22@plicit",
      // "    val ha@23@sh = new java.util.HashMap[Int, Int]()",
      // "    val entries = hash.entry@24@Set()",
      // "    val en@25@try = entries.iterator().next()",
      // "    import java.ut@26@il.Vector",
      // "    import scala.collec@27@tion._",
      // "    val thing: Ex@28@ception = null",
      // "    val ou = Some(1) +@29@+ List(1, 2)",
      // "    List(1, 2).flat@30@Map(Some(_))",
      // "    List(1, 2).coll@31@ect { case 1 => 5 }",
      "    val iPlus = 1 @32@+ 1",
      "  }",
      "  def ar@a1@r1(a: Array[Byte]): Array[Byte] = null",
      "  def ar@a2@r2(a: Array[Array[Byte]]): Array[Array[Byte]] = null",
      "  def ar@a3@r3(a: Array[Object]): Array[Object] = null",
      "  def ar@a4@r4(a: Array[Any]): Array[Any] = null",
      "  def ar@a5@r5(a: Array[Array[Any]]): Array[Array[Any]] = null",
      "}"
    ) { (p, label, cc) =>
        val fqn = cc.askSymbolFqn(p).getOrElse { fail(label) }

        // convenience methods
        def clazz(pkg: Seq[String], clazz: String) =
          ClassName(PackageName(pkg.toList), clazz)
        def method(pkg: Seq[String], cl: String, name: String, desc: String) =
          MethodName(clazz(pkg, cl), name, DescriptorParser.parse(desc))

        /*
         WARNING: these FQNs have been hand-crafted, we should really
         check them against bytecode versions of the signatures to
         make sure we're asserting on the right thing.
         */
        fqn shouldBe {
          label match {
            case "0" => method(Seq("scala"), "Option", "map", "(Lscala/Function1;)Lscala/Option;")
            case "0.5" => ClassName.PrimitiveBoolean
            case "1" => method(Seq("scala"), "Option", "isDefined", "()Z")
            case "2" => method(Seq("scala"), "Option", "flatMap", "(Lscala/Function1;)Lscala/Option;")
            case "3" => method(Seq("scala"), "Option", "flatten", "(Lscala/Predef/$less$colon$less;)Lscala/Option;")
            case "4" => method(Seq("scala"), "Option", "fold", "(Lscala/Function0;Lscala/Function1;)Ljava/lang/Object;")

            // NOTE: docs resolve to called method, not the implementing class
            //       (e.g. Option.mkString vs TraversableOnce.mkString)
            case "5" => method(Seq("scala", "collection"), "TraversableOnce", "mkString", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;")
            case "6" => method(Seq("scala", "collection"), "TraversableOnce", "mkString", "()Ljava/lang/String;")
            case "7" => method(Seq("scala", "collection"), "TraversableOnce", "mkString", "(Ljava/lang/String;)Ljava/lang/String;")

            case "8" => method(Seq("scala"), "Option", "getOrElse", "(Lscala/Function0;)Ljava/lang/Object;")
            case "9" => method(Seq("scala", "collection"), "IterableLike", "grouped", "(I)Lscala/collection/Iterator;")
            case "10" => method(Seq("scala", "collection", "immutable"), "List$", "empty", "()Lscala/collection/immutable/List;")
            case "10.5" => clazz(Seq("java", "io"), "File")

            case "11" => method(Seq("com", "google", "common", "io"), "Files$", "move", "(Ljava/io/File;Ljava/io/File;)V")
            case "12" => method(Seq("com", "google", "common", "io"), "Files$", "asByteSource", "(Ljava/io/File;)Lcom/google/common/io/ByteSource;")
            case "13" => method(Seq("com", "google", "common", "io"), "Files$", "map", "(Ljava/io/File;Ljava/nio/channels/FileChannel/MapMode;)Ljava/nio/MappedByteBuffer;")
            case "14" => method(Seq("com", "google", "common", "io"), "Files$", "map", "(Ljava/io/File;Ljava/nio/channels/FileChannel/MapMode;J)Ljava/nio/MappedByteBuffer;")
            case "14.5" => method(Seq("scala"), "Array$", "apply", "(Lscala/collection/Seq;Lscala/reflect/ClassTag;)Ljava/lang/Object;")

            case "15" => method(Seq("com", "google", "common", "io"), "Files$", "write", "([BLjava/io/File;)V")

            case "16" => method(Seq("scala"), "Some$", "apply", "(Ljava/lang/Object;)Lscala/Some;")
            case "17" => clazz(Seq("java", "lang"), "String")
            case "18" => ClassName.PrimitiveInt
            case "20" => ???

            // case "21" => Seq("java.lang", "Class"), Some("isInstance(java.lang.Object)"))
            // case "22" => Seq("scala", "Predef$$DummyImplicit$"), None)
            // case "23" => Seq("java.util", "HashMap"), None)
            // case "24" => Seq("java.util", "HashMap"), Some("entrySet()"))
            // case "25" => Seq("java.util", "Map.Entry"), None)
            // case "26" => Seq("java.util", "package"), None)
            // case "27" => Seq("scala.collection", "package"), None)
            // // TODO: Would be nice to be able to inspect a particular constructor. The problem is that
            // // symbolAt returns the type itself when point is in 'File', and it's not totally clear
            // // that's wrong.
            // //            case "28" => FQN("java.io.File", Some("File(java.lang.String, java.lang.String)")
            // case "28" => Seq("scala", "package"), Some("Exception=Exception"))

            // // Check @usecase handling.
            // case "29" => Seq("scala", "Some"), Some("++[B>:A,That](that:scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That"))
            // case "30" =>
            //   val expected = if (scala210)
            //     Seq("scala.collection.immutable", "List"), Some("flatMap[B,That](f:A=>scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That"))
            //   else
            //     Seq("scala.collection.immutable", "List"), Some("flatMap[B,That](f:A=>scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That"))

            //   expected
            // case "31" =>
            //   val expected = if (scala210)
            //     Seq("scala.collection.immutable", "List"), Some("collect[B,That](pf:PartialFunction[A,B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That"))
            //   else
            //     Seq("scala.collection.immutable", "List"), Some("collect[B,That](pf:PartialFunction[A,B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That"))
            //   expected
            case "32" => method(Seq("scala"), "Int", "$plus", "(I)I")

            case "a1" => method(Seq("com", "example"), "Thing", "arr1", "([B)[B")
            case "a2" => method(Seq("com", "example"), "Thing", "arr2", "([[B)[[B")
            case "a3" => method(Seq("com", "example"), "Thing", "arr3", "([Ljava/lang/Object;)[Ljava/lang/Object;")
            case "a4" => method(Seq("com", "example"), "Thing", "arr4", "([Ljava/lang/Object;)[Ljava/lang/Object;")
            case "a5" => method(Seq("com", "example"), "Thing", "arr5", "([[Ljava/lang/Object;)[[Ljava/lang/Object;")

          }
        }
      }

  }
}
