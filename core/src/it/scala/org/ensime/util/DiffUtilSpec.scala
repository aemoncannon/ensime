package org.ensime.util

import java.io.File
import org.scalatest._

class DiffUtilSpec extends WordSpec with Matchers {
  "DiffUtil" should {
    "compare original and revised contents and produce a diff in the unified format" in {
      val originalContent =
        """|line1
           |line2
           |line3"""
          .stripMargin.lines.toSeq

      val revisedContent =
        """|line1
           |new-line2
           |line3"""
          .stripMargin.lines.toSeq
      val a = new File("a").getAbsolutePath()
      val b = new File("b").getAbsolutePath()
      val expectedDiff =
        s"""|--- $a
            |+++ $b
            |@@ -1,3 +1,3 @@
            | line1
            |-line2
            |+new-line2
            | line3""".stripMargin

      val diff = DiffUtil.compareContents(originalContent, revisedContent)

      assert(diff === expectedDiff)
    }
  }
}
