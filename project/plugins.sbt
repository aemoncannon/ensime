scalacOptions ++= Seq("-unchecked", "-deprecation")
ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("com.fommil" % "sbt-sensible" % "2.4.3")

// sbt-ensime is needed for the integration tests
addSbtPlugin("org.ensime" % "sbt-ensime" % "2.5.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly"  % "0.14.6")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.8.0")

addSbtPlugin("com.geirsson"  % "sbt-scalafmt" % "1.4.0")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.6.0-M5")
