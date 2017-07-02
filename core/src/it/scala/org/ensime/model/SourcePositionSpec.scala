// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.model

import org.ensime.api._
import org.ensime.fixture._
import org.ensime.indexer.Default
import org.ensime.indexer.graph.ClassDef
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._
import org.ensime.vfs._

class SourcePositionSpec extends EnsimeSpec
    with SharedEnsimeConfigFixture
    with SharedEnsimeVFSFixture {

  val original = EnsimeConfigFixture.SimpleTestProject

  "SourcePosition" should "resolve FqnSymbols for local files with no line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownFile) should matchPattern {
        case Some(LineSourcePosition(RawFile(_), 0)) =>
      }
    }
  }

  it should "resolve FqnSymbols for local with a line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownFile, Some(100)) should matchPattern {
        case Some(LineSourcePosition(RawFile(_), 100)) =>
      }
    }
  }

  it should "resolve FqnSymbols for archive entries with no line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownJarEntry) should matchPattern {
        case Some(LineSourcePosition(ArchiveFile(_, _), 0)) =>
      }
    }
  }

  it should "resolve FqnSymbols for archive entries with a line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownJarEntry, Some(100)) should matchPattern {
        case Some(LineSourcePosition(ArchiveFile(_, _), 100)) =>
      }
    }
  }

  def knownFile(implicit config: EnsimeConfig): String = {
    val f = scalaMain / "org/example/Foo.scala"
    "file://" + f
  }

  def knownJarEntry(implicit config: EnsimeConfig): String = {
    val scalatest = config.projects.flatMap(_.librarySources).find(
      _.file.toString.contains("scalatest_")
    ).get.file.toAbsolutePath
    "jar:" + scalatest + "!/org/scalatest/FunSpec.scala"
  }

  def lookup(uri: String, line: Option[Int] = None) = {
    withVFS { implicit vfs: EnsimeVFS =>
      val sym = ClassDef("", "", "", Some(uri), line, Default, None, None, None)
      LineSourcePositionHelper.fromFqnSymbol(sym)
    }
  }
}
