// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.nio.file.Path

import akka.event.slf4j.SLF4JLogging
import org.ensime.api.EnsimeFile.Implicits.DefaultCharset
import org.ensime.util.file._
import org.ensime.util.path._

object PortUtil extends SLF4JLogging {

  def port(cacheDir: Path, name: String): Option[Int] = {
    val portPath = cacheDir / name
    if (portPath.exists())
      Some(portPath.readString().trim.toInt)
    else
      None
  }

  def writePort(cacheDir: Path, port: Int, name: String): Unit = {
    val portFile = (cacheDir / name).toFile
    if (!portFile.exists()) {
      portFile.createNewFile()
    }

    portFile.deleteOnExit() // doesn't work on Windows
    portFile.writeString(port.toString)
    // Some clients grep the log waiting for this file to be written - so always write the log message.
    log.info("creating port file: " + portFile)
  }
}
