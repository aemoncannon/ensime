// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.nio.file.{ FileSystems, Paths }

import org.ensime.fixture.SharedEnsimeVFSFixture
import org.ensime.util.EnsimeSpec
import org.ensime.util.path._

import scala.util.Properties.{ jdkHome, javaClassPath }
import scala.util.Try

class ClassfileDepicklerSpec extends EnsimeSpec with SharedEnsimeVFSFixture {
  val scalaLib = javaClassPath.split(java.io.File.pathSeparator)
    .filter(_.contains("scala-library"))
    .headOption
    .getOrElse(throw new Exception("No scala lib on class path!"))
  val fs = FileSystems.newFileSystem(Paths.get(scalaLib), null)

  "ClassfileDepickler" should "not depickle J2SE classes" in {
    val rtPath = Paths.get(jdkHome) / "jre" / "lib" / "rt.jar"
    val fs = FileSystems.newFileSystem(rtPath, null)
    val path = fs.getPath("/java/lang/String.class")
    new ClassfileDepickler(path).scalasig should ===(None)
  }

  it should "support typical Scala classes" in {
    val list = fs.getPath("/scala/collection/immutable/List.class")
    new ClassfileDepickler(list).scalasig shouldBe defined
  }

  it should "not expect anything in companions" in {
    val listCompanion = fs.getPath("scala/collection/immutable/List$.class")
    new ClassfileDepickler(listCompanion).scalasig should ===(None)
  }

  it should "not expect anything in closures" in {
    // scala 2.10/2.11 specific, there will be no "scala/io/Source$$anonfun$1.class" generated under 2.12
    Some(fs.getPath("scala/io/Source$$anonfun$1.class"))
      .filter(_.exists)
      .foreach(p => new ClassfileDepickler(p).scalasig should ===(None))
  }

  it should "find type aliases" in {
    val predef = fs.getPath("scala/Predef.class")
    new ClassfileDepickler(predef).getClasses("scala.Predef$").typeAliases.get(s"scala.Predef$$String") should ===(Some(
      RawType(
        ClassName(PackageName(List("scala")), "Predef$"),
        ClassName(PackageName(List("scala")), s"Predef$$String"),
        "scala.Predef.String",
        Public,
        " = java.lang.String"
      )
    ))
  }
}
