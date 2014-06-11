/**
 *  Copyright (c) 2010, Aemon Cannon
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of ENSIME nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ensime.server

import java.io.OutputStream
import scala.collection.mutable.ArrayBuffer

sealed trait LogParserState
case object Initial extends LogParserState
case class SourceLine(severity: String, file: String, line: Int, message: String) extends LogParserState
case class MessageLine(severity: String, file: String, line: Int, message: String) extends LogParserState

trait CompilerReportHandler {
  def report(report: CompilerReport): Unit
}
case class CompilerReport(severity: String, file: String, line: Int, col: Int, message: String)

class CompilerLogParser(reporter: CompilerReportHandler, out: Option[OutputStream]) extends OutputStream {
  var remainingInput = ""
  var currentState: LogParserState = Initial

  override def write(b: Array[Byte], off: Int, len: Int) = {
    synchronized {
      out foreach { _.write(b, off, len) }
      val input = new String(b, off, len) // TODO: Support different encodings
      val (r, s) = process(remainingInput + input, currentState)
      remainingInput = r
      currentState = s
    }
  }

  override def write(b: Int) = {
    synchronized {
      out foreach { _.write(b) }
      val input = b.toChar
      if(input == '\n') {
        val (r, s) = process(remainingInput + input, currentState)
        remainingInput = r
        currentState = s
      } else remainingInput += input
    }
  }

  val SourceLineR = """^\[(\w+)\] (\S+):(\d+): (.*\n)$""".r
  val MessageLineR = """^\[(\w+)\] (.*\n)$""".r
  val CarretLineR = """^\[(\w+)\] ( +\^\n)$""".r

  private def process(input: String, currentState: LogParserState): (String, LogParserState) = {
    val (line, rest) = nextLine(input)
    if (line.isEmpty) (rest, currentState)
    else {
      val nextState =
        currentState match {
          case Initial =>
            line match {
              case SourceLineR(sev, file, line, msg) => SourceLine(sev, file, line.toInt, msg)
              case _ => Initial
            }
          case SourceLine(s, f, l, m) =>
            line match {
              case CarretLineR(sev, spaces) if s == sev =>
                val report = CompilerReport(s, f, l, spaces.length-2, m)
                reporter.report(report)
                Initial
              case MessageLineR(sev, msg) if s == sev => MessageLine(s, f, l, m + msg)
              case "" => SourceLine(s, f, l, m)
              case _ => Initial
            }
          case MessageLine(s, f, l, m) =>
            line match {
              case CarretLineR(sev, spaces) if s == sev =>
                val report = CompilerReport(s, f, l, spaces.length-2, m + spaces)
                reporter.report(report)
                Initial
              case MessageLineR(sev, msg) if s == sev => MessageLine(s, f, l, m + msg)
              case "" => MessageLine(s, f, l, m)
              case _ => Initial
            }
        }
      process(rest, nextState)
    }
  }

  def nextLine(input: String): (String, String) = {
    val eol = input.indexOf('\n')
    if (eol >= 0) input.splitAt(eol + 1)
    else ("", input)
  }
}
