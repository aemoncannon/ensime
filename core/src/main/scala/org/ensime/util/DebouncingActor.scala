// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import akka.actor.{ Actor, ActorLogging, Cancellable }

import scala.concurrent.duration.FiniteDuration

object DebouncingActor {
  case object Debounce
  case object Perform
  case object Cancel
}

class DebouncingActor(delayedAction: () => Unit, delayDuration: FiniteDuration) extends Actor with ActorLogging {
  import DebouncingActor._
  import context.system

  private var worker: Cancellable = _

  private def debounce(): Unit = {
    cancel()
    import context.dispatcher
    worker = system.scheduler.scheduleOnce(delayDuration, self, Perform)
  }

  private def perform(): Unit = {
    cancel()
    delayedAction()
  }

  private def cancel(): Unit = Option(worker).foreach(_.cancel())

  override def receive: Receive = {
    case Debounce => debounce()
    case Perform => perform()
    case Cancel => cancel()
  }
}
