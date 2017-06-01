package org.ensime.util

import java.nio.file._
import scala.util.Properties.jdkHome
import org.ensime.util.path._

object TestPaths {
  lazy val src: Path = Paths.get(jdkHome.stripSuffix("/jre")) / "src.zip"
}
