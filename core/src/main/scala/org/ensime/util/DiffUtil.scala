package org.ensime.util

object DiffUtil {

  def compareContents(original: Seq[String], revised: Seq[String], originalName: String = "a", revisedName: String = "b"): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(originalName, revisedName, original.asJava, diff, 1).asScala.mkString("\n")
  }

}
