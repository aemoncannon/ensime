package org.ensime.test

import org.ensime.protocol.MessageTracker
import org.ensime.util.SExp

class NoOpMessageTracker extends MessageTracker {
  override def call(sexp: SExp, callId: Int): Unit = {}

  override def result(exp: SExp, callId: Int): Unit = {}

  override def error(msg: String, callId: Int): Unit = {}
}
