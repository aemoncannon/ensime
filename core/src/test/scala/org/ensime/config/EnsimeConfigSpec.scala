// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import org.ensime.util.file._
import org.ensime.util.{ EnsimeSpec, EscapingStringInterpolation }

import org.ensime.api._
import org.ensime.config.richconfig._

import scala.util.Properties

class EnsimeConfigSpec extends EnsimeSpec {

  import EscapingStringInterpolation._

  def test(contents: String, testFn: (EnsimeConfig) => Unit): Unit = {
    val config = EnsimeConfigProtocol.parse(contents)
    assert(config.isRight)
    testFn(config.right.get)
  }

  "EnsimeConfig" should "parse a simple config" in withTempDir { dir =>
    val abc      = dir / "abc"
    val cache    = dir / ".ensime_cache"
    val javaHome = File(Properties.javaHome)

    abc.mkdirs()
    cache.mkdirs()

    test(
      s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home "$javaHome"
 :root-dir "$dir"
 :cache-dir "$cache"
 :java-sources ()
 :projects ((:id (:project "module1" :config "compile")
             :depends ()
             :sources ()
             :targets ("$abc")
             :scalac-options ("wibble" "wobble")
             :javac-options ("flibble" "flobble")
             :library-jars ("$javaHome/src.zip")
             :library-sources ("$javaHome/src.zip")
             :library-docs ("$javaHome/src.zip"))))""", { implicit config =>
        config.name shouldBe "project"
        config.scalaVersion shouldBe "2.10.4"
        val module1 = config.lookup(EnsimeProjectId("module1", "compile"))
        module1.id.project shouldBe "module1"
        module1.dependencies shouldBe empty
        config.projects.size shouldBe 1

        val proj = config.projects.head
        proj.scalacOptions should not be empty
        proj.javacOptions should not be empty
        proj.libraryJars should not be empty
        proj.librarySources should not be empty
        proj.libraryDocs should not be empty
      }
    )
  }

  it should "parse a minimal config for a binary only project" in withTempDir {
    dir =>
      val abc      = dir / "abc"
      val cache    = dir / ".ensime_cache"
      val javaHome = File(Properties.javaHome)

      abc.mkdirs()
      cache.mkdirs()

      test(
        s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home "$javaHome"
 :root-dir "$dir"
 :cache-dir "$cache"
 :projects ((:id (:project "module1" :config "compile")
             :depends ()
             :targets ("$abc"))))""", { implicit config =>
          config.name shouldBe "project"
          config.scalaVersion shouldBe "2.10.4"
          val module1 = config.lookup(EnsimeProjectId("module1", "compile"))
          module1.id.project shouldBe "module1"
          module1.dependencies shouldBe empty
          module1.targets should have size 1
        }
      )
  }
}
