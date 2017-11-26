// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import java.io.OutputStream

import akka.event.slf4j.SLF4JLogging
import spray.json._

/**
 * A class to write Json RPC messages on an output stream, following the Language Server Protocol.
 * It produces the following format:
 *
 * <Header> '\r\n' <Content>
 *
 * Header := FieldName ':' FieldValue '\r\n'
 *
 * Currently there are two defined header fields:
 * - 'Content-Length' in bytes (required)
 * - 'Content-Type' (string), defaults to 'application/vscode-jsonrpc; charset=utf8'
 *
 * @note The header part is defined to be ASCII encoded, while the content part is UTF8.
 */
class MessageWriter(out: OutputStream) extends SLF4JLogging {

  /** Lock protecting the output stream, so multiple writes don't mix message chunks. */
  private val lock = new Object

  /**
   * Write a message to the output stream. This method can be called from multiple threads,
   * but it may block waiting for other threads to finish writing.
   */
  def write[T](msg: T)(implicit o: JsWriter[T]): Unit = lock.synchronized {
    val str          = CompactPrinter(o.write(msg))
    val contentBytes = str.getBytes(MessageReader.Utf8Charset)
    val headers =
      s"""Content-Length: ${contentBytes.length}\r
         |\r
         |""".stripMargin

    log.debug(s"$headers$str")

    val headerBytes = headers.getBytes(MessageReader.AsciiCharset)

    out.write(headerBytes)
    out.write(contentBytes)
    out.flush()
  }
}
