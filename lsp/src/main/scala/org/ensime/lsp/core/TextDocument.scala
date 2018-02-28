// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import java.io.File
import java.net.URI

import org.ensime.lsp.api.types._

final case class TextDocument(uri: String, contents: Array[Char]) {
  def applyChanges(
    changes: Seq[TextDocumentContentChangeEvent]
  ): TextDocument = {
    // we assume full text sync
    assert(changes.size == 1)
    val change = changes.head
    assert(change.range.isEmpty)
    assert(change.rangeLength.isEmpty)

    copy(contents = change.text.toArray)
  }

  private[this] def peek(idx: Int): Int =
    if (idx < contents.length) contents(idx).toInt else -1

  def toFile: File = new File(URI.create(uri))

  /**
   * Return the corresponding position in this text document as 0-based line and column.
   */
  def offsetToPosition(offset: Int): Position = {
    if (offset >= contents.length)
      throw new IndexOutOfBoundsException(
        s"$uri: asked position at offset $offset, but contents is only ${contents.length} characters long."
      )

    var i    = 0
    var line = 0
    var col  = 0

    while (i < offset) {
      contents(i) match {
        case '\r' =>
          line += 1
          col = 0
          if (peek(i + 1) == '\n') i += 1

        case '\n' =>
          line += 1
          col = 0

        case _ =>
          col += 1
      }
      i += 1
    }

    Position(line, col)
  }

  /**
   * Return the offset in the current document, for a given 0-based line/col position.
   */
  def positionToOffset(pos: Position): Int = {
    val Position(line, col) = pos

    var i = 0
    var l = 0

    while (i < contents.length && l < line) {
      contents(i) match {
        case '\r' =>
          l += 1
          if (peek(i + 1) == '\n') i += 1

        case '\n' =>
          l += 1

        case _ =>
      }
      i += 1
    }

    if (l < line)
      throw new IllegalArgumentException(
        s"$uri: Can't find position $pos in contents of only $l lines long."
      )
    if (i + col < contents.length)
      i + col
    else
      throw new IllegalArgumentException(
        s"$uri: Invalid column. Position $pos in line '${contents.slice(i, contents.length).mkString}'"
      )
  }

  def lineToOffset(lineNr: Int): Int =
    positionToOffset(Position(lineNr, 0))

}
