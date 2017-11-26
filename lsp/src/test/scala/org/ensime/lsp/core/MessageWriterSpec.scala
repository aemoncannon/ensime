// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import java.io.{ PipedInputStream, PipedOutputStream }

import org.scalatest.Matchers._
import org.scalatest._
import spray.json._
import JsWriter.ops._
import JsReader.ops._

class MessageWriterSpec extends FreeSpec with BeforeAndAfter {

  var inStream: PipedInputStream   = _
  var outStream: PipedOutputStream = _

  before {
    inStream = new PipedInputStream
    outStream = new PipedOutputStream(inStream)
  }

  after {
    outStream.close()
    inStream.close()
  }

  "simple message" - {
    "should be written and read correctly" in {
      val msgWriter = new MessageWriter(outStream)
      val msgReader = new MessageReader(inStream)

      val obj = Map("field1" -> Seq(1, 2, 3),
                    "field2" -> Seq(4, 5, 6),
                    "field3" -> Seq(10, 1000, 100000))
      msgWriter.write(obj)

      val Some(payload) = msgReader.nextPayload()
      val resObj        = JsParser(payload).as[Map[String, Seq[Int]]]
      resObj shouldEqual obj
    }
  }

  "long message" - {
    "should be written and read correctly" in {
      val msgWriter = new MessageWriter(outStream)
      val msgReader = new MessageReader(inStream)

      val obj = (1 to 100000).map(i => s"field$i" -> i.toBinaryString).toMap
      msgWriter.write(obj)

      val Some(payload) = msgReader.nextPayload()
      val resObj        = JsParser(payload).as[Map[String, String]]
      resObj shouldEqual obj
    }
  }
}
