// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import java.nio.file.{ Files, Paths }

import org.ensime.api.ArchiveFile
import org.scalatest._
import org.scalatest.Matchers._

class TempFileStoreSpec extends FreeSpec {
  "Extraction in TempFileStore" - {
    "should work correctly" in {
      val tempStore = new TempFileStore(
        Files.createTempDirectory("testStore").toAbsolutePath.toString
      )
      val jar = Paths.get(getClass.getResource("/test.jar").toURI)
      val extracted = tempStore.getFile(
        ArchiveFile(jar, "/scala/Predef.scala")
      )

      extracted.isSuccess shouldBe true
    }
  }
}
