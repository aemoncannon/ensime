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
      def inputStreamToByteArray(is: InputStream): Array[Byte] = {
        Iterator continually is.read takeWhile (-1 !=) map (_.toByte) toArray
      }
      inputStreamToByteArray(is)
    }
  }

  implicit class RichOutputStream(val os: OutputStream) extends AnyVal {
    /**
     * Copy the input stream to the output stream, making best
     * endeavours to close everything afterward (even on failure).
     */
    def drain(in: InputStream): Unit =
      try {
        var buffer = Array.fill[Byte](1024)(0) // size does affect perfomace
        var len: Int = 0
        def read(): Int = { len = in.read(buffer); len }
        while (read != -1) {
          os.write(buffer, 0, len)
        }
      } finally {
        try in.close()
        finally os.close()
      }
  }

}

