// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import java.io.InputStream
import java.nio.charset.Charset

import akka.event.slf4j.SLF4JLogging

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object MessageReader {
  private[lsp] val BufferSize            = 8192
  private[lsp] val AsciiCharset: Charset = Charset.forName("ASCII")
  private[lsp] val Utf8Charset: Charset  = Charset.forName("UTF-8")
}

/**
 * A Language Server message Reader. It expects the following format:
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
class MessageReader(in: InputStream) extends SLF4JLogging {
  private val buffer = new Array[Byte](MessageReader.BufferSize)
  @volatile
  private var data = ArrayBuffer.empty[Byte]
  @volatile
  private var streamClosed = false

  private val lock = new Object

  private class PumpInput extends Thread("Input Reader") {
    override def run(): Unit = {
      var nRead = 0
      do {
        nRead = in.read(buffer)
        if (nRead > 0) lock.synchronized {
          data ++= buffer.slice(0, nRead)
          lock.notify()
        }
      } while (nRead > 0)
      log.info("End of stream, terminating thread")
      lock.synchronized {
        streamClosed = true
        lock.notify() // some threads might be still waiting for input
      }
    }
  }

  (new PumpInput).start()

  /**
   * Return headers, if any are available. It returns only full headers, after the
   *
   * \r\n\r\n mark has been seen.
   *
   * @return A map of headers. If the map is empty it could be that the input stream
   *         was closed, or there were no headers before the delimiter. You can disambiguate
   *         by checking {{{this.streamClosed}}}
   */
  private[core] final def getHeaders: Map[String, String] = lock.synchronized {
    def atDelimiter(idx: Int): Boolean =
      (data.size >= idx + 4
        && data(idx) == '\r'
        && data(idx + 1) == '\n'
        && data(idx + 2) == '\r'
        && data(idx + 3) == '\n')

    while (data.size < 4 && !streamClosed) lock.wait()

    if (!streamClosed) {
      var i = 0
      while (i + 4 < data.size && !atDelimiter(i)) {
        i += 1
      }

      if (atDelimiter(i)) {
        val headers =
          new String(data.slice(0, i).toArray, MessageReader.AsciiCharset)
        log.debug(s"Received headers:\n$headers")

        val pairs = headers.split("\r\n").filter(_.trim.length() > 0) map {
          line =>
            line.split(":") match {
              case Array(key, value) => Some(key.trim -> value.trim)
              case _ =>
                log.error(s"Malformed input: $line")
                None
            }
        }

        // drop headers
        data = data.drop(i + 4)

        // if there was a malformed header we keep trying to re-sync and read again

        if (pairs.contains(None)) {
          log.error(
            s"There was an empty pair in ${pairs.toSeq}, trying to read another header."
          )
          getHeaders
        } else pairs.flatten.toMap
      } else if (streamClosed) {
        Map.empty
      } else {
        lock.wait()
        getHeaders
      }
    } else {
      Map.empty
    }
  }

  /**
   * Return `len` bytes of content as a string encoded in UTF8.
   *
   * @note If the stream was closed this method returns the empty string.
   */
  private[core] def getContent(len: Int): Option[String] = lock.synchronized {
    while (data.size < len && !streamClosed) lock.wait()

    if (streamClosed) {
      None
    } else {
      assert(data.size >= len)
      val content = data.take(len).toArray
      data = data.drop(len)
      Some(new String(content, MessageReader.Utf8Charset))
    }
  }

  /**
   * Return the next JSON RPC content payload. Blocks until enough data has been received.
   */
  def nextPayload(): Option[String] =
    if (streamClosed) None
    else {
      // blocks until headers are available
      val headers = getHeaders

      if (headers.isEmpty && streamClosed) {
        None
      } else {
        headers
          .get("Content-Length")
          .flatMap(s => Try { s.toInt }.toOption)
          .flatMap(getContent)
          .orElse {
            log.error(
              "Input must have Content-Length header with a numeric value."
            )
            nextPayload()
          }
      }
    }

}
