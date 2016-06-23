package org.ensime.util

/**
 * Created by harsh on 16/06/16.
 */
import java.io.FileInputStream
import java.net.{ JarURLConnection, URL }
import java.nio.file.Paths
import java.security.MessageDigest
import org.ensime.util.path._

object PathUtils {
  def getAbsolutePath(path: Path): Path = if (!path.isJarUrl) path.toAbsolutePath
  else {
    val split = path.toString.split("!").take(2)
    val slashOrNot = if (path.toString.startsWith("/")) "/" else ""
    Paths.get(Paths.get(split.head.split("jar:file:")(1)).getParent.toString + slashOrNot + split(1))
  }
  def baseName(path: String) = splitPath(path, front = false)

  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else if (front) path.substring(0, idx + 1)
    else path.substring(idx + 1)
  }
  def generateMd5(fileName: String) = {
    val plaintext = fileName
    val m = MessageDigest.getInstance("MD5")
    m.reset()
    m.update(plaintext.getBytes())
    val digest = m.digest()
    val bigInt = BigInt(1, digest)
    val hashtext = bigInt.toString(16)
    (0 to (32 - hashtext.length)).map(v => "0").mkString("") + hashtext
  }
  def getInputStream(path: Path) = if (!path.isJarUrl) new FileInputStream(path.toFile) else {
    val inputFile = path.toString
    val inputURL = new URL(inputFile)
    val conn = inputURL.openConnection().asInstanceOf[JarURLConnection]
    conn.getInputStream
  }
}
