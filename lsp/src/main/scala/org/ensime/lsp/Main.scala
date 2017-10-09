package org.ensime.lsp

import scala.util.Properties
import java.io.PrintStream
import java.io.FileOutputStream

import org.ensime.lsp.ensime.EnsimeLanguageServer
import org.ensime.util.Slf4jSetup
import org.slf4j._

object Main extends App {
  Slf4jSetup.init()

  val log = LoggerFactory.getLogger("LSP")

  val cwd = Option(System.getProperty("lsp.workspace")).getOrElse(".") // TODO load config
  log.info(s"Starting server in $cwd")
  log.info(s"Classpath: ${Properties.javaClassPath}")

  val server = new EnsimeLanguageServer(System.in, System.out)

  // route System.out somewhere else. The presentation compiler may spit out text
  // and that confuses VScode, since stdout is used for the language server protocol
  val origOut = System.out
  try {
    System.setOut(
      new PrintStream(new FileOutputStream(s"$cwd/pc.stdout.log"))
    )
    System.setErr(
      new PrintStream(new FileOutputStream(s"$cwd/pc.stdout.log"))
    )
    println("This file contains stdout from the presentation compiler.")
    server.start()
  } finally {
    System.setOut(origOut)
  }

  // make sure we actually exit
  System.exit(0)
}
