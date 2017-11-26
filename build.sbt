lazy val monkeys   = EnsimeBuild.monkeys
lazy val util      = EnsimeBuild.util
lazy val testutil  = EnsimeBuild.testutil
lazy val json      = EnsimeBuild.json
lazy val s_express = EnsimeBuild.s_express
lazy val api       = EnsimeBuild.api
lazy val swanky    = EnsimeBuild.swanky
lazy val core      = EnsimeBuild.core
lazy val server    = EnsimeBuild.server
lazy val lsp       = EnsimeBuild.lsp

lazy val testingEmpty     = EnsimeTestingBuild.testingEmpty
lazy val testingSimple    = EnsimeTestingBuild.testingSimple
lazy val testingSimpleJar = EnsimeTestingBuild.testingSimpleJar
lazy val testingImplicits = EnsimeTestingBuild.testingImplicits
lazy val testingTiming    = EnsimeTestingBuild.testingTiming
lazy val testingMacros    = EnsimeTestingBuild.testingMacros
lazy val testingShapeless = EnsimeTestingBuild.testingShapeless
lazy val testingFqns      = EnsimeTestingBuild.testingFqns
lazy val testingDebug     = EnsimeTestingBuild.testingDebug
lazy val testingDocs      = EnsimeTestingBuild.testingDocs
lazy val testingJava      = EnsimeTestingBuild.testingJava

lazy val root = EnsimeBuild.root

// WORKAROUND: until https://github.com/scalameta/scalafmt/issues/1081
commands += Command.args("fmt", "scalafmt CLI") {
  case (state, args) =>
    val Right(scalafmt) =
      org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion("1.3.0-16-49815ab4")
    scalafmt.main(
      List(
        "--config",
        "project/scalafmt.conf",
        "--git",
        "true",
        "--exclude",
        "testing",
        "--non-interactive"
      ) ++: args
    )
    state
}
