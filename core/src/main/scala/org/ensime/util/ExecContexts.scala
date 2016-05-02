// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.util.concurrent._

import scala.concurrent.ExecutionContext

object BoundedExecutor {

  /**
   * This is a bounded execution context that will block the calling thread until
   * a worker is available. The work queue is only sized to a single element because
   * this causes a failure when attempting to add a work element, which in turn will
   * fire the rejected execution handler, causing the calling thread to block.
   */

  //unsafe 
  def callerBlockingExecutor(size: Int): ExecutionContext = {
    val q = new LinkedBlockingQueue[Runnable](1)
    val h = new ThreadPoolExecutor.CallerRunsPolicy()
    val ex = new ThreadPoolExecutor(size, size, 30, TimeUnit.SECONDS, q, h)
    ExecutionContext.fromExecutor(ex)
  }
}
