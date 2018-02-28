// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io._

/**
 * NOTE: prefer NIO via the path utilities.
 */
package object io {

  implicit class RichInputStream(private val is: InputStream) extends AnyVal {
    def toByteArray(): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val data = Array.ofDim[Byte](16384)

      var len: Int    = 0
      def read(): Int = { len = is.read(data, 0, data.length); len }

      while (read != -1) {
        baos.write(data, 0, len)
      }

      baos.toByteArray()
    }
  }

  implicit class RichOutputStream(private val os: OutputStream) extends AnyVal {

    /**
     * Copy the input stream to the output stream, making best
     * endeavours to close everything afterward (even on failure).
     */
    def drain(in: InputStream): Unit =
      try {
        val data = Array.ofDim[Byte](16384) // size does affect perfomance

        var len: Int    = 0
        def read(): Int = { len = in.read(data, 0, data.length); len }

        while (read != -1) {
          os.write(data, 0, len)
        }
      } finally {
        try in.close()
        finally os.close()
      }
  }

}
