// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import java.io.File
import java.net.URI
import java.util.UUID

class StandardFormatsSpec extends FormatSpec {

  "StandardFormats" should "support Option" in {
    val some = Some("thing")
    assertFormat(some: Option[String], SexpString("thing"))
    assertFormat(None: Option[String], SexpNil)
  }

  it should "support Either" in {
    val left  = Left(13)
    val right = Right("thirteen")
    assertFormat(
      left: Either[Int, String],
      SexpInteger(13)
    )
    assertFormat(
      right: Either[Int, String],
      SexpString("thirteen")
    )
  }

  it should "support UUID" in {
    val uuid = UUID.randomUUID()
    assertFormat(uuid, SexpString(uuid.toString))
  }

  it should "support URI" in {
    val github = "http://github.com/ensime/"
    val url    = new URI(github)
    assertFormat(url, SexpString(github))
  }

  it should "support File" in {
    val file = new File("foo")
    assertFormat(file, SexpString("foo"))
  }

}
