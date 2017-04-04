// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.{ File => JFile, _ }
import java.nio.charset.Charset
import java.util.regex.Pattern
import java.nio.file.Files
import scala.io.Source

import scala.util.Try

import org.ensime.api.deprecating

/**
 * Decorate `java.io.File` with functionality from common utility
 * packages, which would otherwise be verbose/ugly to call directly.
 *
 * Its nicer to put conveniences for working with `File` here
 * instead of using static accessors from J2SE or Guava.
 *
 * NOTE: prefer NIO via the path utilities.
 */
package object file {
  type File = JFile

  /**
   * Convenience for creating `File`s (which we do a lot), but has the
   * caveat that static methods on `java.io.File` can no longer be
   * accessed, so it must be imported like:
   *
   *   `java.io.{ File => JFile }`
   */
  def File(name: String): File = new File(name)

  def withTempDir[T](a: File => T): T = {
    val dir = Files.createTempDirectory("ensime").toFile.canon
    import path._
    try a(dir)
    finally Try(dir.toPath.deleteDirRecursively())
  }

  def withTempFile[T](a: File => T): T = {
    val file = Files.createTempFile("ensime-", ".tmp").toFile.canon
    try a(file)
    finally Try(file.delete())
  }

  implicit class RichFile(val file: File) extends AnyVal {

    def /(sub: String): File = new File(file, sub)

    def isScala: Boolean = file.getName.toLowerCase.endsWith(".scala")
    def isJava: Boolean = file.getName.toLowerCase.endsWith(".java")
    def isClassfile: Boolean = file.getName.toLowerCase.endsWith(".class")
    def isJar: Boolean = file.getName.toLowerCase.endsWith(".jar")

    def parts: List[String] =
      file.getPath.split(
        Pattern.quote(JFile.separator)
      ).toList.filterNot(Set("", "."))

    def outputStream(): OutputStream = new FileOutputStream(file)

    def createWithParents(): Boolean = {
      file.getParentFile().mkdirs()
      file.createNewFile()
    }

    def readLines()(implicit cs: Charset): List[String] = {
      Source.fromFile(file.getPath).getLines.toList
    }

    def writeLines(lines: List[String])(implicit cs: Charset): Unit = {
      Files.write(file.toPath(), lines.mkString("", "\n", "\n").getBytes(cs))
    }

    def writeString(contents: String)(implicit cs: Charset): Unit = {
      Files.write(file.toPath(), contents.getBytes(cs))
    }

    @deprecating("prefer path")
    def readString()(implicit cs: Charset): String = {
      import path._
      file.toPath.readString
    }

    /**
     * @return the file and its descendent family tree (if it is a directory).
     */
    @deprecating("prefer path approaches")
    def tree: Stream[File] = {
      import scala.annotation.tailrec

      def listFiles(file: File): Array[File] = {
        @tailrec
        def recursiveListFiles(files: List[File], result: List[File]): List[File] = files match {
          case Nil => result
          case head :: tail if head.isDirectory =>
            recursiveListFiles(Option(head.listFiles).map(_.toList ::: tail).getOrElse(tail), head +: result)
          case head :: tail if head.isFile =>
            recursiveListFiles(tail, head :: result)
        }
        recursiveListFiles(List(file), Nil).toArray
      }

      def fileTreeTraverser(f: File): Stream[File] = {
        listFiles(f).reverse.toStream
      }

      file.isDirectory match {
        case true => fileTreeTraverser(file)
        case false => file #:: fileTreeTraverser(file)
      }
    }

    /**
     * Non-recursive children of the file.
     */
    def children: Stream[File] =
      Option(file.listFiles()).map(_.toStream).getOrElse(Stream.empty)

    /**
     * Helps to resolve ambiguity surrounding files in symbolically
     * linked directories, which are common on operating systems that
     * use a symbolically linked temporary directory (OS X I'm looking
     * at you).
     *
     * @return the canonical form of `file`, falling back to the absolute file.
     */
    def canon =
      try file.getCanonicalFile
      catch {
        case t: Throwable => file.getAbsoluteFile
      }

  }

}
