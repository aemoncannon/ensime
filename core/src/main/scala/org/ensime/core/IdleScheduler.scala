package org.ensime.core

import akka.actor.{ Actor, ActorRef, Cancellable }
import scala.concurrent.duration._

case object Idle
/*
Actor that sends Idle  message every times seconds to Project actor.
 */
class IdleScheduler(project: ActorRef) extends Actor {

  import context.{ dispatcher, system }

  var idleCallback: Option[Cancellable] = None

  override def preStart(): Unit = {
    idleCallback = Some(context.system.scheduler.schedule(5.seconds, 5.seconds, project, Idle))
  }

  override def postStop(): Unit = {
    idleCallback.foreach(_.cancel)
  }

  def receive: Receive = {
    case _ =>
  }

}
