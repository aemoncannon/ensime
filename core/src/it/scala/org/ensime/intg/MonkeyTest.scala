// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import org.apache.commons.io.FileUtils
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.scalatest.concurrent.Eventually
import scala.concurrent.duration._

/**
 * Tests a project that uses jars instead of classfiles in the target,
 * ensuring that we can delete the jars when the presentation compiler
 * is running (this is expected to always pass on Linux, but requires
 * the monkeys for Windows).
 */
class MonkeyTest extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture {

  val original = EnsimeConfigFixture.SimpleJarTestProject

  "ensime-server" should "allow jar targets to be deleted" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        withProject { (project, asyncHelper) =>
          mainTarget should be a 'file

          // no scaling here
          eventually(timeout(30 seconds), interval(1 second)) {
            mainTarget.delete() shouldBe true
          }
        }
      }
    }
  }

}
