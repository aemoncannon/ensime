// Copyright (C) 2015 Sam Halliday
// License: see the LICENSE file
package org.ensime.util

import com.google.common.base.Charsets
import java.io._
import com.google.common.io.Files

import Predef.{ any2stringadd => _, _ => _ }
import java.nio.charset.Charset
import java.util.regex.Pattern

/**
 * Decorate `java.io.File` with functionality from common utility
 * packages, which would be otherwise verbose to call directly.
 */
package object file {
  type File = java.io.File

  // TODO: rename to File (capital)
  def file(name: String): File = new File(name)

  implicit val DefaultCharset: Charset = Charset.defaultCharset()

  /**
   * WARNING: do not create symbolic links in the temporary directory
   * or the cleanup script will exit the sandbox and start deleting
   * other files.
   */
  def withTempDir[T](a: File => T): T = {
    // sadly not able to provide a prefix. If we really need the
    // support we could re-implement the Guava method.
    val dir = Files.createTempDir().canon
    try a(dir)
    finally dir.tree.reverse.foreach(_.delete())
  }

  def withTempFile[T](a: File => T): T = {
    val file = File.createTempFile("ensime-", ".tmp").canon
    try a(file)
    finally file.delete()
  }

  implicit class RichFile(val file: File) extends AnyVal {

    def /(sub: String): File = new File(file, sub)

    def parts: List[String] =
      file.getPath.split(
        Pattern.quote(File.separator)
      ).toList.filterNot(Set("", "."))

    def outputStream(): OutputStream = new FileOutputStream(file)

    def createWithParents(): Unit = {
      Files.createParentDirs(file)
      file.createNewFile()
    }

    def readLines()(implicit cs: Charset): List[String] = {
      import collection.JavaConversions._
      Files.readLines(file, cs).toList
    }

    def writeLines(lines: List[String])(implicit cs: Charset): Unit = {
      Files.write(lines.mkString("", "\n", "\n"), file, cs)
    }

    def readString()(implicit cs: Charset): String = {
      Files.toString(file, cs)
    }

    /**
     * @return the file and its descendent family tree (if it is a directory).
     */
    def tree: Stream[File] = {
      import collection.JavaConversions._
      file #:: Files.fileTreeTraverser().breadthFirstTraversal(file).toStream
    }

    /**
     * Helps to resolve ambiguity surrounding files in symbolically
     * linked directories (which are common on operating systems that
     * use a symbolically linked temporary directory).
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
