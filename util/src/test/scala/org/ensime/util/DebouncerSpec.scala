// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.time.{ Clock, Instant, ZoneId }
import org.scalatest._
import scala.concurrent.duration._

class DebouncerSpec extends FlatSpec with Matchers {
  class TestScheduler(clock: Clock) extends Debouncer.SchedulerLike {
    @volatile var action: () => Unit   = _
    @volatile var nextInvocation: Long = Long.MaxValue
    @volatile var currentTime: Long    = 0L
    override def scheduleOnce(delay: FiniteDuration)(body: () => Unit): Unit =
      synchronized {
        require(nextInvocation == Long.MaxValue,
                "Error! Only one item can be scheduled at a time")
        action = body
        nextInvocation = currentTime + delay.toMillis
      }

    def poll(): Unit = synchronized {
      currentTime = clock.millis()
      if (currentTime >= nextInvocation) {
        nextInvocation = Long.MaxValue
        action()
      }
    }
  }
  class TestClock extends Clock {
    @volatile var currentTime: Long = 0
    def advanceTime(by: Long): Unit = synchronized {
      require(by > 0)
      currentTime += by
    }

    override def instant(): Instant     = Instant.ofEpochMilli(currentTime)
    override def getZone(): ZoneId      = java.time.ZoneOffset.UTC
    override def withZone(zone: ZoneId) = ???
  }

  case class DebouncerFixture(debounceFirst: Boolean = true) {
    val clock     = new TestClock
    val scheduler = new TestScheduler(clock)
    def advanceTime(amount: FiniteDuration): Unit = {
      clock.advanceTime(amount.toMillis)
      scheduler.poll()
    }
    var called   = false
    val delay    = 5.seconds
    val maxDelay = 20.seconds
    lazy val debouncer =
      Debouncer("test", scheduler, delay, maxDelay, debounceFirst, clock) {
        () =>
          called = true
      }
  }

  it should "call the action only after the delay has passed" in {
    val f = DebouncerFixture()
    f.debouncer.call()
    f.called.shouldBe(false)
    f.advanceTime(f.delay)
    f.called.shouldBe(true)
  }

  it should "extend the deadline if called before the delay has passed" in {
    val f = DebouncerFixture()
    f.debouncer.call()
    f.called.shouldBe(false)
    f.advanceTime(2.seconds)
    f.called.shouldBe(false)

    f.debouncer.call()
    f.advanceTime(4.seconds)
    f.called.shouldBe(false)

    f.advanceTime(f.delay)
    f.called.shouldBe(true)
  }

  it should "call after maxDelay has passed" in {
    val f = DebouncerFixture()
    (1 to 20).foreach { _ =>
      f.debouncer.call()
      f.called.shouldBe(false)
      f.advanceTime(1.second)
    }
    f.called.shouldBe(true)
  }

  it should "call immediately if debounceFirst = false" in {
    val f = DebouncerFixture(debounceFirst = false)
    f.debouncer.call()
    f.called.shouldBe(true)
    f.called = false
    f.debouncer.call()
    f.called.shouldBe(false)
    f.advanceTime(f.delay)
    f.called.shouldBe(true)
  }
}
