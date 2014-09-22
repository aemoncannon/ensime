package org.ensime.protocol

import java.util.Date

import akka.actor.{ Props, ActorSystem, Actor, ActorLogging }
import org.ensime.util.SExp
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object MessageTracker {
  case class RPCCall(exp: SExp, callId: Int)
  case class RPCError(msg: String, callId: Int)
  case class RPCResponse(exp: SExp, callId: Int)
  case class MessageCheck(callId: Int, sendTime: Long)
  class MessageTrackingActor(msgTimeout: FiniteDuration) extends Actor with ActorLogging {

    var outstanding: Map[Int, RPCCall] = Map.empty

    override def receive: Receive = {
      case call @ RPCCall(exp, callId) =>
        outstanding += (callId -> call)
        implicit val ex: ExecutionContext = context.dispatcher
        context.system.scheduler.scheduleOnce(msgTimeout, self, MessageCheck(callId, System.currentTimeMillis()))
      case RPCError(_, callId) =>
        if (outstanding.contains(callId)) {
          log.warning("RPC Error sent for call: " + callId)
          outstanding -= callId
        } else
          log.error("RPC error sent for call: " + callId)
      case RPCResponse(_, callId) =>
        if (!outstanding.contains(callId))
          log.error("Got response for expired (or incorrect) RPC call: " + callId)
        else {
          outstanding -= callId
        }
      case MessageCheck(callId, sendTime) =>
        if (outstanding.contains(callId)) {
          log.error("No response sent from call " + callId + "(" + new Date(sendTime) +
            ") - call is :" + outstanding(callId).exp)
          outstanding -= callId
        }
    }
  }
}

trait MessageTracker {
  def call(sexp: SExp, callId: Int): Unit
  def result(exp: SExp, callId: Int): Unit
  def error(msg: String, callId: Int): Unit
}

class ReportingMessageTracker(actorSystem: ActorSystem, msgTimeout: FiniteDuration) extends MessageTracker {
  import MessageTracker._
  protected val actor = actorSystem.actorOf(Props(new MessageTrackingActor(msgTimeout)))

  def call(sexp: SExp, callId: Int): Unit = {
    actor ! RPCCall(sexp, callId)
  }

  def result(exp: SExp, callId: Int): Unit = {
    actor ! RPCResponse(exp, callId)
  }

  def error(msg: String, callId: Int): Unit = {
    actor ! RPCError(msg, callId)
  }
}
