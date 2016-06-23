package org.ensime.util

import java.net.{ JarURLConnection, URL }
import java.nio.charset.Charset
import java.nio.file.{ Path => JPath }

import com.google.common.io.Files
/**
 * Created by harsh on 15/06/16.
 */
package object path {
  type Path = JPath
  implicit class EnrichPath(val path: Path) extends AnyVal {
    def isJarUrl = path.toString.startsWith("jar:file:")
    def isJava: Boolean = path.toString.endsWith(".java")
    def isScala: Boolean = path.toString.endsWith(".scala")
    def readString(implicit cs: Charset): String = {
      if (!path.isJarUrl) Files.toString(path.toFile, cs) else {
        val in = new URL(path.toString).openConnection().asInstanceOf[JarURLConnection].getInputStream
        try {
          scala.io.Source.fromInputStream(in).mkString
        } finally {
          in.close()
        }
      }
    }
    def toURI = path.toUri
  }
}
