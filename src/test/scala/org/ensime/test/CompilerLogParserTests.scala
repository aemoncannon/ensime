package org.ensime.test

import org.scalatest.WordSpec
//import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Matchers
import org.scalamock.scalatest.MockFactory
import org.ensime.server._

class CompilerLogParserTests extends WordSpec with Matchers with MockFactory {
  "a log parser" should {
    "parse a single warning message" in {
      val input = """[warn] /path/to/file/source.scala:123: A warning message
[warn] a line of code
[warn]   ^
"""
      val expected = CompilerReport("warn", "/path/to/file/source.scala", 123, 3, "A warning message\na line of code\n  ^\n")
      val handler = mock[CompilerReportHandler]
      (handler.report _) expects(expected)
      val parser = new CompilerLogParser(handler, None)
      val bytes = input.getBytes
      parser.write(bytes, 0, bytes.length)
    }

    "parse multiple messages" in {
      val input = """[warn] /path/to/file/source.scala:123: A warning message
[warn] a line of code
[warn]   ^
[error] /path/to/file/source2.scala:456: An error message
[error]   Error detail line 1
[error]   Error detail line 2
[error] some scala code
[error]      ^
"""
      val expected1 = CompilerReport("warn", "/path/to/file/source.scala", 123, 3, "A warning message\na line of code\n  ^\n")
      val expected2 = CompilerReport("error", "/path/to/file/source2.scala", 456, 6, "An error message\n  Error detail line 1\n  Error detail line 2\nsome scala code\n     ^\n")
      val handler = mock[CompilerReportHandler]
      (handler.report _) expects(expected1)
      (handler.report _) expects(expected2)
      val parser = new CompilerLogParser(handler, None)
      val bytes = input.getBytes
      parser.write(bytes, 0, bytes.length)
    }

    "parse multiple messages byte-by-byte" in {
      val input = """[warn] /path/to/file/source.scala:123: A warning message
[warn] a line of code
[warn]   ^
[error] /path/to/file/source2.scala:456: An error message
[error]   Error detail line 1
[error]   Error detail line 2
[error] some scala code
[error]      ^
"""
      val expected1 = CompilerReport("warn", "/path/to/file/source.scala", 123, 3, "A warning message\na line of code\n  ^\n")
      val expected2 = CompilerReport("error", "/path/to/file/source2.scala", 456, 6, "An error message\n  Error detail line 1\n  Error detail line 2\nsome scala code\n     ^\n")
      val handler = mock[CompilerReportHandler]
      (handler.report _) expects(expected1)
      (handler.report _) expects(expected2)
      val parser = new CompilerLogParser(handler, None)
      val bytes = input.getBytes
      bytes foreach { b => parser.write(b.toInt) }
    }
  }
}
