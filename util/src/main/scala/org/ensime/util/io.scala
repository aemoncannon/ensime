// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io._

/**
 * NOTE: prefer NIO via the path utilities.
 */
package object io {

  implicit class RichInputStream(val is: InputStream) extends AnyVal {
    def toByteArray(): Array[Byte] = {
      val baos = new ByteArrayOutputStream()

      val data = Array.ofDim[Byte](16384)
      var nRead = is.read(data, 0, data.length)

      while (nRead != -1) {
        baos.write(data, 0, nRead)
        nRead = is.read(data, 0, data.length)
      }

      baos.flush()
      baos.toByteArray
    }
  }
}
