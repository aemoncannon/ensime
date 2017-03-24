// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io._
import java.net._

import org.apache.commons.vfs2.FileObject
import org.ensime.api.deprecating
import org.ensime.util.ensimefile._

/**
 * Adds functionality to the Apache VFS FileObject.
 */
package object fileobject {

  @deprecating("https://github.com/ensime/ensime-server/issues/1437")
  implicit class RichFileObject(val fo: FileObject) extends AnyVal {
    // None if the fo is not an entry in an archive
    def pathWithinArchive: Option[String] = {
      val uriS = uriString
      if (uriS.startsWith("jar") || uriS.startsWith("zip"))
        Some(fo.getName.getRoot.getRelativeName(fo.getName))
      else None
    }

    // assumes it is a local file
    def asLocalFile: File = new File(uri)

    // variant of .getURI that is more likely to agree with EnsimeFile.uri
    def uri: URI = EnsimeFile(fo.getName.getURI).uri

    def uriString: String = uri.toASCIIString
  }

}
