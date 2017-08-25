// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.nio.file.Paths

import org.ensime.api.{ EnsimeFile, RawFile }
import org.ensime.fixture.IsolatedEnsimeVFSFixture
import org.ensime.util.EnsimeSpec

import scala.collection.immutable.Queue

class ClassfileIndexerSpec extends EnsimeSpec with IsolatedEnsimeVFSFixture {

  def indexClassfile(f: EnsimeFile) = new ClassfileIndexer(f).indexClassfile()

  //TODO: Remove
  "ClassfileIndexer" should "support Java 6 class files" in {
    val path = getClass.getResource("/jdk6/Test.class").toURI
    val clazz = indexClassfile(RawFile(Paths.get(path)))
    clazz.name shouldBe ClassName(PackageName(Nil), "Test")
    clazz.generics shouldBe None
    clazz.superClass shouldBe Some(ClassName(PackageName(List("java", "lang")), "Object"))
    clazz.interfaces shouldBe Nil
    clazz.access shouldBe Default
    clazz.deprecated shouldBe false
    clazz.fields shouldBe List()
    var methods = clazz.methods
    methods should matchPattern {
      case Queue(
        RawMethod(
          MethodName(
            ClassName(PackageName(Nil), "Test"),
            "<init>",
            Descriptor(Nil, ClassName(PackageName(Nil), "void"))
            ),
          Default,
          None,
          Some(1),
          _
          ),
        RawMethod(
          MethodName(
            ClassName(PackageName(Nil), "Test"),
            "main",
            Descriptor(List(ArrayDescriptor(ClassName(PackageName(List("java", "lang")), "String"))), ClassName(PackageName(Nil), "void"))
            ),
          Public,
          None,
          Some(4),
          _
          )
        ) =>
    }
    methods.head.internalRefs should contain theSameElementsAs List(
      FullyQualifiedReference(ClassName(PackageName(List("java", "lang")), "Object"), Some(1)),
      FullyQualifiedReference(MethodName(
        ClassName(PackageName(List("java", "lang")), "Object"),
        "<init>",
        Descriptor(Nil, ClassName(PackageName(Nil), "void"))
      ), Some(1)),
      FullyQualifiedReference(ClassName(PackageName(Nil), "void"), Some(1))
    )
    methods = methods.tail
    methods.head.internalRefs should contain theSameElementsAs List(
      FullyQualifiedReference(ClassName(PackageName(Nil), "void"), Some(3)),
      FullyQualifiedReference(FieldName(ClassName(PackageName(List("java", "lang")), "System"), "out"), Some(3)),
      FullyQualifiedReference(ClassName(PackageName(List("java", "io")), "PrintStream"), Some(3)),
      FullyQualifiedReference(ClassName(PackageName(List("java", "lang")), "String"), Some(4)),
      FullyQualifiedReference(ClassName(PackageName(List("java", "lang")), "String"), Some(3)),
      FullyQualifiedReference(MethodName(
        ClassName(PackageName(List("java", "io")), "PrintStream"),
        "print",
        Descriptor(List(ClassName(PackageName(List("java", "lang")), "String")), ClassName(PackageName(Nil), "void"))
      ), Some(3)),
      FullyQualifiedReference(ClassName(PackageName(List()), "void"), Some(4))
    )
    clazz.source shouldBe RawSource(Some("Test.java"), Some(1))
    val refs = clazz.internalRefs.map(_.fqn) ++ clazz.methods.flatMap(_.internalRefs.map(_.fqn)) ++ clazz.fields.flatMap(_.internalRefs.map(_.fqn))

    refs.distinct should contain theSameElementsAs List(
      ClassName(PackageName(Nil), "void"),
      ClassName(PackageName(List("java", "lang")), "Object"),
      FieldName(ClassName(PackageName(List("java", "lang")), "System"), "out"),
      ClassName(PackageName(List("java", "io")), "PrintStream"),
      MethodName(
        ClassName(PackageName(List("java", "lang")), "Object"),
        "<init>",
        Descriptor(Nil, ClassName(PackageName(Nil), "void"))
      ),
      MethodName(
        ClassName(PackageName(List("java", "io")), "PrintStream"),
        "print",
        Descriptor(List(ClassName(PackageName(List("java", "lang")), "String")), ClassName(PackageName(Nil), "void"))
      ),
      ClassName(PackageName(List("java", "lang")), "String")
    )
  }

  it should "support Java 8 class files" in {
    val test = getClass.getResource("/jdk8/Test.class").toURI
    indexClassfile(RawFile(Paths.get(test)))

    val annotation = getClass.getResource("/jdk8/MyAnnotation.class").toURI
    indexClassfile(RawFile(Paths.get(annotation)))

    val inner = getClass.getResource("/jdk8/Test$InnerClassWithCtorParam.class").toURI
    indexClassfile(RawFile(Paths.get(inner)))
  }

}
