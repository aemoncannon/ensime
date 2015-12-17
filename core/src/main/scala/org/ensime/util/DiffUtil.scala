package org.ensime.util

import java.io.File

object DiffUtil {

  def compareContents(original: Seq[String], revised: Seq[String], originalFile: File = new File("a"), revisedFile: File = new File("b")): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    val originalName = originalFile.getAbsolutePath()
    val revisedName = revisedFile.getAbsolutePath()
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(originalName, revisedName, original.asJava, diff, 1).asScala.mkString("\n")
  }
}
