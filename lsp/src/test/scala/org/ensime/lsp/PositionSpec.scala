package org.ensime.lsp

import org.scalatest._

import scala.meta.lsp.Position

final class PositionToOffsetSpec extends FlatSpec with EitherValues with Matchers {

  val uri = "file:///test/file.scala"

  "positionToOffset" should "convert positions in a text document with a single line" in {
    val text = "Foo"
    val position = Position(line = 0, character = 0)
    val offset = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset.right.value shouldBe 0
  }

  it should "count newline characters (\\n)" in {
    val text = "Foo\nBar"
    val position = Position(line = 1, character = 0)
    val offset = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset.right.value shouldBe 4
  }

  it should "double count Windows-style newline characters (\\r\\n)" in {
    val text = "Foo\r\nBar"
    val position = Position(line = 1, character = 0)
    val offset = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset.right.value shouldBe 5
  }

  it should "validate that the position is on a line within the file line count" in {
    val text = "Foo"
    val position = Position(line = 1, character = 0)
    val offset = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'left
  }

  it should "validate that the position is on a character within the character count of the position's line" in {
    val text = "Foo"
    val position = Position(line = 0, character = 3)
    val offset = LspToEnsimeAdapter.positionToOffset(uri, text, position)

    offset shouldBe 'left
  }
}
