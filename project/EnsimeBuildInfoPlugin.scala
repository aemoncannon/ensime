import scala.util._

import sbt._
import sbt.IO._
import sbt.Keys._

import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._
import sbtdynver.DynVerPlugin, DynVerPlugin.autoImport._

object EnsimeBuildInfoPlugin extends AutoPlugin {
  override def requires = BuildInfoPlugin && DynVerPlugin
  override def trigger  = allRequirements

  override val projectSettings = Seq(
    buildInfoPackage := organization.value,
    buildInfoKeys += BuildInfoKey.action("gitSha")(
      dynverGitDescribeOutput.value.map(_.ref.value).getOrElse("n/a")
    ),
    buildInfoKeys += BuildInfoKey.action("builtAtString")(currentDateString())
  )

  private def currentDateString() = {
    val dtf = new java.text.SimpleDateFormat("yyyy-MM-dd")
    dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    dtf.format(new java.util.Date())
  }
}
