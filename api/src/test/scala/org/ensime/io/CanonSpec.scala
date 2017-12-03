// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.io

import java.io.File
import java.nio.file._

import scala.util.Properties.jdkHome

import scalaz.std.list._

import org.scalatest._
import org.scalatest.Matchers._

import org.ensime.api._

import Canon.ops._

class CanonSpec extends FlatSpec {

  lazy val file  = new File(".")
  lazy val canon = file.canon.unsafePerformIO()

  def Canonised[A: Canon](a: A): A = a.canon.unsafePerformIO()

  "Canon" should "canon File" in {
    Canonised(file) shouldBe canon
  }

  it should "canon List of Files" in {
    Canonised(List(file)) shouldBe List(canon)
  }

  class MyFile(name: String) extends File(name)

  it should "canon subtypes of File when used in File position" in {
    val mine: File = new MyFile(".")
    Canonised(mine) should not be (mine)
  }

  it should "canon an RpcRequest" in {
    val request  = TypeAtPointReq(Left(file), OffsetRange(100)): RpcRequest
    val expected = TypeAtPointReq(Left(canon), OffsetRange(100))
    Canonised(request) shouldBe expected
  }

  it should "canon an EnsimeServerMessage" in {
    val response = Breakpoint(RawFile(file.toPath), 13): RpcResponse
    val expected = Breakpoint(RawFile(canon.toPath), 13)
    Canonised(response) shouldBe expected
  }

  // NOTE: doesn't delete contents
  def withTempDir[T](a: File => T): T = {
    val dir = Files.createTempDirectory("ensime").toFile
    try a(dir)
    finally dir.delete()
  }

  it should "canon a RawFile" in withTempDir { dir =>
    val rawDir   = RawFile(dir.toPath)
    val ef       = List(RawFile(file.toPath))
    val expected = List(RawFile(canon.toPath))

    Canonised(ef) shouldBe expected
  }

  it should "canon an ArchiveFile" in withTempDir { dir =>
    val rawDir = RawFile(dir.toPath)
    val src    = Paths.get(s"$jdkHome/src.zip")

    val entry = EnsimeFile(s"$src!/java/lang/String.java")
    val expected =
      ArchiveFile(src.canon.unsafePerformIO, "/java/lang/String.java")

    Canonised(List(entry)) shouldBe List(expected)
  }

}
