scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet
libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.fommil" % "sbt-sensible" % "1.2.0")
addSbtPlugin("com.lucidchart" % "sbt-scalafmt-coursier" % "1.12")

// sbt-ensime is needed for the integration tests
addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.13")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.5.1") // 1.8.0 causes https://github.com/sbt/sbt-header/issues/56
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5" exclude("org.apache.maven", "maven-plugin-api"))
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
