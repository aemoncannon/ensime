lazy val api = project
  .dependsOn(json, `s-express`)
  .settings(
    licenses := Seq(LGPL3),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-effect" % "7.2.17"
    )
  )

lazy val json = project.settings(
  licenses := Seq(LGPL3),
  libraryDependencies ++= Seq(
    "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
  ) ++ shapeless.value
)

lazy val `s-express` = project
  .settings(
    licenses := Seq(LGPL3),
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "fastparse"  % "0.4.4",
      "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
    ) ++ shapeless.value
  )

lazy val monkeys = project
  .disablePlugins(ScalafixPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-vfs2" % "2.1"
    )
  )

lazy val util = project
  .dependsOn(api, monkeys)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka"        %% "akka-actor"    % akkaVersion,
      "org.scala-lang"           % "scala-compiler" % scalaVersion.value,
      "com.google.code.findbugs" % "jsr305"         % "3.0.2" % "provided"
    ) ++ logback ++ shapeless.value
  )

lazy val testutil = project
  .dependsOn(util)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j"   % akkaVersion
    ) ++ sensibleTestLibs(Compile)
  )

lazy val core = project
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(
    api,
    `s-express`,
    monkeys,
    util,
    api      % "test->test", // for the interpolator
    testutil % "test,it",
    // depend on "it" dependencies in Test or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    testingEmpty  % "test,it",
    testingSimple % "test,it",
    // test config needed to get the test jar
    testingSimpleJar % "test,it->test",
    testingTiming    % "test,it",
    testingMacros    % "test,it",
    testingShapeless % "test,it",
    testingJava      % "test,it"
  )
  .enableIntegrationTests
  .settings(
    unmanagedJars in Compile += JavaTools,
    ensimeUnmanagedSourceArchives += (baseDirectory in ThisBuild).value / "openjdk-langtools/openjdk8-langtools-src.zip",
    libraryDependencies ++= Seq(
      "com.orientechnologies" % "orientdb-graphdb" % orientVersion
        exclude ("commons-collections", "commons-collections")
        exclude ("commons-beanutils", "commons-beanutils"),
      "org.apache.lucene" % "lucene-core"             % luceneVersion,
      "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
      "org.ow2.asm"       % "asm-commons"             % "5.2",
      "org.ow2.asm"       % "asm-util"                % "5.2",
      "org.scala-lang"    % "scalap"                  % scalaVersion.value,
      "com.typesafe.akka" %% "akka-actor"             % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j"             % akkaVersion, {
        // see notes in https://github.com/ensime/ensime-server/pull/1446
        val suffix = CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 11)) => "2.11.8"
          case _             => "2.12.2"
        }
        "org.scala-refactoring" % s"org.scala-refactoring.library_${suffix}" % "0.13.0"
      },
      "com.googlecode.java-diff-utils" % "diffutils"           % "1.3.0"
    ) ++ shapeless.value
  )

lazy val server = project
  .dependsOn(
    core,
    lsp,
    `s-express` % "test->test",
    // depend on "it" dependencies in Test or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    core        % "test->test",
    core        % "it->it",
    testingDocs % "test,it"
  )
  .enableIntegrationTests
  .settings(
    libraryDependencies ++= Seq(
      "io.netty" % "netty-transport"  % nettyVersion,
      "io.netty" % "netty-handler"    % nettyVersion,
      "io.netty" % "netty-codec-http" % nettyVersion
    ) ++ shapeless.value
  )

lazy val lsp = project
  .dependsOn(core, json)

// the projects used in integration tests
lazy val testingEmpty = testingProject("testing/empty")
lazy val testingSimple = testingProject("testing/simple") settings (
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test intransitive ()
)
lazy val testingSimpleJar = testingProject("testing/simpleJar").settings(
  exportJars := true,
  ensimeUseTarget in Compile := Some(
    (artifactPath in (Compile, packageBin)).value
  ),
  ensimeUseTarget in Test := Some((artifactPath in (Test, packageBin)).value)
)
lazy val testingImplicits = testingProject("testing/implicits")
lazy val testingTiming    = testingProject("testing/timing")
lazy val testingMacros = testingProject("testing/macros") settings (
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
lazy val testingShapeless = testingProject("testing/shapeless").settings(
  libraryDependencies ++= shapeless.value
)
lazy val testingDocs = testingProject("testing/docs").settings(
  dependencyOverrides ++= Seq("com.google.guava" % "guava" % "18.0"),
  libraryDependencies ++= Seq(
    "com.github.dvdme" % "ForecastIOLib" % "1.5.1" intransitive (),
    "com.google.guava" % "guava"         % "18.0"
  )
)
lazy val testingJava = testingProject("testing/java").settings(
  crossPaths := false,
  autoScalaLibrary := false,
  libraryDependencies := Nil
)

// root project
name := "ensime"
dependsOn(server)
publishLocal := {}
publish := {}
test in assembly := {}
aggregate in assembly := false
assemblyMergeStrategy in assembly := {
  case PathList("org", "apache", "commons", "vfs2", xs @ _*) =>
    MergeStrategy.first
  case PathList("META-INF", "io.netty.versions.properties") =>
    MergeStrategy.concat
  case PathList("LICENSE")         => MergeStrategy.concat
  case PathList("LICENSE.apache2") => MergeStrategy.first
  case PathList("NOTICE")          => MergeStrategy.concat
  case other                       => MergeStrategy.defaultMergeStrategy(other)
}
assemblyExcludedJars in assembly := {
  val everything = (fullClasspath in assembly).value
  everything.filter { attr =>
    val n = attr.data.getName
    n.startsWith("scala-library") | n.startsWith("scala-compiler") |
      n.startsWith("scala-reflect") | n.startsWith("scalap")
  } :+ Attributed.blank(JavaTools)
}
assemblyJarName in assembly := s"ensime_${scalaBinaryVersion.value}-${version.value}-assembly.jar"

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

TaskKey[Unit](
  "prewarm",
  "Uses this build to create a cache, speeding up integration tests"
) := {
  // would be good to be able to do this without exiting the JVM...
  val sv = scalaVersion.value
  val cmd =
    if (sys.env.contains("APPVEYOR")) """C:\sbt\bin\sbt.bat"""
    else if (sys.env.contains("TRAVIS")) "../../sbt"
    else "sbt"
  sys.process
    .Process(
      Seq(cmd, s"++$sv!", "ensimeConfig", "ensimeServerIndex"),
      file("testing/cache")
    )
    .!
}

addCommandAlias("check", ";fmt --test")
addCommandAlias("prep", ";ensimeConfig ;assembly ;prewarm")
addCommandAlias("cpl", "all compile test:compile it:compile")
addCommandAlias("tests", "all test it:test")
// not really what is used in CI, but close enough...
addCommandAlias("ci", "all check prep cpl doc tests")
