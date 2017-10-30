// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import org.ensime.api.ArchiveFile
import org.scalatest._
import org.scalatest.Matchers._

import scala.reflect.io.Path

class TempFileStoreSpec extends FreeSpec {
  "Extraction in TempFileStore" - {
    "should work correctly" in {
      val tempStore = new TempFileStore("/tmp/testStore")

      // we don't know if we are run inside the root project, or the sub-project
      val jar  = Path("src") / "test" / "resources" / "test.jar"
      val jar1 = if (jar.exists) jar else Path("lsp") / jar

      val extracted = tempStore.getFile(
        ArchiveFile(jar1.toAbsolute.jfile.toPath, "/scala/Predef.scala")
      )

      extracted.isSuccess shouldBe true
    }
  }
}
