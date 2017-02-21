// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.database

import javax.sql.DataSource

object SlickBackCompat {
  val h2Api = slick.driver.H2Driver.api

  import h2Api._
  def forDataSource(ds: DataSource, executor: AsyncExecutor) = Database.forDataSource(ds, executor)

}
