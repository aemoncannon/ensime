package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.scalatest.{ BeforeAndAfterAll, FunSpec, Matchers }

class ClassfileIndexerSpec extends FunSpec with Matchers with ClassfileIndexer with SLF4JLogging
    with BeforeAndAfterAll {

  var vfs: EnsimeVFS = _

  override def beforeAll(): Unit = {
    vfs = EnsimeVFS()
  }

  override def afterAll(): Unit = {
    vfs.close()
  }

  // TODO: some assertions (currently we're just checking that no exceptions are raised!)

  describe("ClassfileIndexer") {
    it("should support Java 6 class files") {
      indexClassfile(vfs.vres("jdk6/Test.class"))
    }

    it("should support Java 8 class files") {
      indexClassfile(vfs.vres("jdk8/Test.class"))
      indexClassfile(vfs.vres("jdk8/MyAnnotation.class"))
      indexClassfile(vfs.vres("jdk8/Test$InnerClassWithCtorParam.class"))
    }

    it("should support typical J2SE classes") {
      val (clazz, refs) = indexClassfile(vfs.vres("java/lang/String.class"))
      assert(clazz.access === Public)
    }

    it("should support typical Scala classes") {
      indexClassfile(vfs.vres("scala/collection/immutable/List.class"))
      indexClassfile(vfs.vres("scala/collection/immutable/List$.class"))
    }

    it("should support method overloading") {
      val (clazz, _) = indexClassfile(vfs.vres("java/nio/channels/FileChannel.class"))
      val methods = clazz.methods.filter { ref => ref.name.fqnString.startsWith("java.nio.channels.FileChannel.write") }

      methods.map(_.name.fqnString) should contain theSameElementsAs List(
        "java.nio.channels.FileChannel.write(Ljava/nio/ByteBuffer;)I",
        "java.nio.channels.FileChannel.write([Ljava/nio/ByteBuffer;II)J",
        "java.nio.channels.FileChannel.write([Ljava/nio/ByteBuffer;)J",
        "java.nio.channels.FileChannel.write(Ljava/nio/ByteBuffer;J)I"
      )
    }

  }
}
