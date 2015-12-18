package org.ensime.util

import org.ensime.api._

object FileEditHelper {

  import scala.tools.refactoring.common.{ Change, NewFileChange, TextChange }

  def fromChange(ch: Change): FileEdit = {
    ch match {
      case ch: TextChange => TextEdit(ch.file.file, ch.from, ch.to, ch.text)
      case _ => throw new UnsupportedOperationException(ch.toString)
    }
  }

  def applyEdits(ch: List[TextEdit], source: String): String = {
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }

  def diffFromTextEdits(ch: List[TextEdit], source: String, originalName: String, revisedName: String): String = {
    val newContents = applyEdits(ch, source)
    DiffUtil.compareContents(source.lines.toSeq, newContents.lines.toSeq, originalName, revisedName)
  }

  //TODO: add diffFromNewFile and diffFromDeleteFile
  //def diffFromNewFile(ch: NewFile, source: String): String = ???
  //def diffFromDeleteFile(ch: DeleteFile, source: String): String = ??

}
