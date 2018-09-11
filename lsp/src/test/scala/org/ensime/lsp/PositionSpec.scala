package org.ensime.lsp

import org.scalatest._

import scala.meta.lsp.Position

final class PositionSpec extends FlatSpec with EitherValues with Matchers {

  val uri = "file:///test/file.scala"

  "positionToOffset" should "convert positions in a text document with a single line" in {
    val text     = "Foo"
    val position = Position(line = 0, character = 0)
    val offset   = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'right
    offset.right.value shouldBe 0
  }

  it should "count newline characters (\\n)" in {
    val text     = "Foo\nBar"
    val position = Position(line = 1, character = 0)
    val offset   = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'right
    offset.right.value shouldBe 4
  }

  it should "double count Windows-style newline characters (\\r\\n)" in {
    val text     = "Foo\r\nBar"
    val position = Position(line = 1, character = 0)
    val offset   = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'right
    offset.right.value shouldBe 5
  }

  it should "validate that the position is on a line within the file line count" in {
    val text     = "Foo"
    val position = Position(line = 1, character = 0)
    val offset   = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'left
  }

  it should "validate that the position is on a character within the character count of the position's line" in {
    val text     = "Foo"
    val position = Position(line = 0, character = 3)
    val offset   = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'left
  }

  "offsetToPosition" should "convert offsets to positions in a text document with a single line" in {
    val text     = "Foo"
    val offset   = 2
    val position = EnsimeToLspAdapter.offsetToPosition(uri, text, offset)

    position shouldBe 'right
    position.right.value shouldBe Position(line = 0, character = 2)
  }

  it should "convert offsets to positions in a text document with multiple lines" in {
    val text     = "Foo\nBar"
    val offset   = 4
    val position = EnsimeToLspAdapter.offsetToPosition(uri, text, offset)

    position shouldBe 'right
    position.right.value shouldBe Position(line = 1, character = 0)
  }

  it should "convert offsets to positions for newline characters" in {
    // Surely positions of newline characters are nonsensical
    val text     = "Foo\nBar"
    val offset   = 3
    val position = EnsimeToLspAdapter.offsetToPosition(uri, text, offset)

    position shouldBe 'right
    position.right.value shouldBe Position(line = 0, character = 3)
  }
}
