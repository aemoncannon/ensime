// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import Predef.{ any2stringadd => _ }
import java.io.ByteArrayInputStream

import org.scalatest._

class IoSpec extends FlatSpec with Matchers {
  import io._

  val bytes = Array[Byte](0, 1, -2, 127, -128)

  "io._" should "convert to a byte array" in {
    val in = new ByteArrayInputStream(bytes)
    in.toByteArray() shouldEqual bytes
  }
}
