// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import akka.actor.{ ActorContext, ActorRef, Scheduler }
import java.time.Clock
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }
import org.slf4j.LoggerFactory
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
 * Highly performant generic debouncer
 *
 * Note: This is not written as an Actor for performance reasons. Debounced calls are filtered synchronously, in the
 * caller thread, without the need for locks, context switches, or heap allocations. We use AtomicBoolean to resolve
 * concurrent races; the probability of contending on an AtomicBoolean transition is very low.
 *
 * @param name The name of this debouncer
 * @param scheduler The scheduler implementation to use; Can accept an akka scheduler a la magnet pattern
 * @param delay The time after which to invoke action; each time [[Debouncer.call]] is invoked, this delay is reset
 * @param maxDelay The maximum time to delay the invocation of action()
 * @param action The action to invoke
 * @param debounceFirst If false, then the first invocation of call() calls action exactly once. Subsequent calls are debounced.
 * @param Clock The source from which we get the current time. This input should use the same source. Specified for testing purposes
 */
class Debouncer(
  name: String,
  scheduler: Debouncer.SchedulerLike,
  delay: FiniteDuration,
  maxDelay: FiniteDuration,
  action: () => Unit,
  debounceFirst: Boolean = true,
  clock: Clock = Clock.systemUTC
) {
  import Debouncer.logger
  require(delay <= maxDelay, "delay should be <= maxDelay")
  private[this] val pending                         = new AtomicBoolean(false)
  private[this] val calls                           = new AtomicInteger(0)
  private[this] val debouncingActive                = new AtomicBoolean(debounceFirst)
  private val delayMs                               = delay.toMillis
  private val maxDelayMs                            = maxDelay.toMillis
  @volatile private[this] var lastCallAttempt: Long = clock.millis()
  @volatile private[this] var lastInvocation: Long  = clock.millis()

  /**
   * Register that the provided action should be called according to the debounce logic
   */
  def call(): Unit =
    if (activateDebouncing) {
      action()
    } else {
      lastCallAttempt = clock.millis()
      calls.incrementAndGet()
      if (pending.compareAndSet(false, true)) {
        scheduler.scheduleOnce(delay) { () =>
          tryActionOrPostpone()
        }
      }
    }

  /** Returns true if debouncing was inactive, and we just activated it */
  private[this] def activateDebouncing =
    debouncingActive.compareAndSet(false, true)

  /** See if it's time to invoke the debounce action */
  private[this] def tryActionOrPostpone(): Unit = {
    val now               = clock.millis()
    val delaySurpassed    = ((now - lastCallAttempt) >= delayMs)
    val maxDelaySurpassed = ((now - lastInvocation) >= maxDelayMs)

    if (delaySurpassed || maxDelaySurpassed) {
      lastInvocation = now
      if (pending.compareAndSet(true, false)) {
        val foldedCalls = calls.getAndSet(0)
        if (maxDelaySurpassed)
          logger.info(
            s"Debouncer action $name invoked after maxDelay $maxDelay surpassed ($foldedCalls calls)"
          )
        else if (logger.isDebugEnabled)
          logger.debug(
            s"Debouncer action $name invoked after delay $delay surpassed ($foldedCalls calls)"
          )
        try action()
        catch {
          case ex: Throwable =>
            logger.error(s"Debouncer ${name} action resulted in error", ex)
        }
      } else
        logger.error(
          s"Invalid state in debouncer. Should not have reached here!"
        )
    } else {
      // reschedule at the earliest of origLastInvocation + delayMs or origLastCallAttempt + maxDelayMs
      // Note: we use Math.max as there's a _very_ small chance lastCallAttempt could advance in another thread, and
      // result in a negative calcaulation
      val delay = Math.max(1,
                           Math.min((lastCallAttempt - now + delayMs),
                                    (lastInvocation - now + maxDelayMs)))
      scheduler.scheduleOnce(delay.millis) { () =>
        tryActionOrPostpone()
      }
    }
  }
}

object Debouncer {
  private val logger = LoggerFactory.getLogger(getClass)

  def apply(
    name: String,
    scheduler: Debouncer.SchedulerLike,
    delay: FiniteDuration,
    maxDelay: FiniteDuration,
    debounceFirst: Boolean = true,
    clock: Clock = Clock.systemUTC
  )(action: () => Unit): Debouncer =
    new Debouncer(name,
                  scheduler,
                  delay,
                  maxDelay,
                  action,
                  debounceFirst,
                  clock)

  /**
   * Debouncer which uses sends the specified message to the specified actor.
   *
   * Uses scheduler and dispatcher implicitly from the actor context.
   */
  def forActor(
    actorRef: ActorRef,
    message: Any,
    delay: FiniteDuration,
    maxDelay: FiniteDuration,
    name: String = null,
    debounceFirst: Boolean = true,
    clock: Clock = Clock.systemUTC
  )(implicit context: ActorContext): Debouncer = {
    import context.dispatcher
    new Debouncer(
      Option(name).getOrElse {
        s"${context.self.path.name} -> ${actorRef.path.name}"
      },
      context.system.scheduler,
      delay,
      maxDelay, { () =>
        actorRef ! message
      },
      debounceFirst,
      clock
    )
  }

  trait SchedulerLike {

    /**
     * Scheduler method which is guaranteed to call body AFTER the provided delay passes
     */
    def scheduleOnce(delay: FiniteDuration)(body: () => Unit): Unit
  }

  object SchedulerLike {
    implicit def fromAkkaScheduler(scheduler: Scheduler)(
      implicit
      ec: ExecutionContext
    ): SchedulerLike = new SchedulerLike {
      def scheduleOnce(delay: FiniteDuration)(body: () => Unit): Unit =
        scheduler.scheduleOnce(delay)(body())
    }
  }
}
