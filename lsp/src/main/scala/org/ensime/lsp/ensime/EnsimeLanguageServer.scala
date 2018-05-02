// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.ensime

import java.io.{ File, InputStream, OutputStream }
import java.net.URI
import java.nio.file.Paths

import akka.actor.{ ActorRef, ActorSystem, Props }
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config._
import org.ensime.api._
import org.ensime.config.EnsimeConfigProtocol
import org.ensime.config.richconfig._
import org.ensime.core._
import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.types._
import org.ensime.lsp.core.{ LanguageServer, MessageReader, TextDocument }
import org.ensime.util.file._
import org.ensime.util.path._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }

object EnsimeLanguageServer {
  private val KeywordToKind = Map(
    "class"   -> SymbolKind.Class,
    "trait"   -> SymbolKind.Interface,
    "type"    -> SymbolKind.Interface,
    "package" -> SymbolKind.Package,
    "def"     -> SymbolKind.Method,
    "val"     -> SymbolKind.Constant,
    "var"     -> SymbolKind.Field
  )

  private def toSourceFileInfo(
    uri: String,
    contents: Option[String] = None
  ): SourceFileInfo = {
    val f = new File(new URI(uri))
    SourceFileInfo(RawFile(f.toPath), contents)
  }

  private def toCompletion(completionInfo: CompletionInfo) = {
    def symKind: Option[Int] = completionInfo.typeInfo map { info =>
      info.declaredAs match {
        case DeclaredAs.Method => CompletionItemKind.Method
        case DeclaredAs.Class  => CompletionItemKind.Class
        case DeclaredAs.Field  => CompletionItemKind.Field
        case DeclaredAs.Interface | DeclaredAs.Trait =>
          CompletionItemKind.Interface
        case DeclaredAs.Object => CompletionItemKind.Module
        case _                 => CompletionItemKind.Value
      }
    }

    CompletionItem(
      label = completionInfo.name,
      kind = symKind,
      detail = completionInfo.typeInfo.map(_.fullName)
    )
  }

  private def toDiagnostic(note: Note): Diagnostic = {
    val start  = note.beg
    val end    = note.end
    val length = end - start

    val severity = note.severity match {
      case NoteError => DiagnosticSeverity.Error
      case NoteWarn  => DiagnosticSeverity.Warning
      case NoteInfo  => DiagnosticSeverity.Information
    }

    // Scalac reports 1-based line and columns, while Code expects 0-based
    val range = Range(Position(note.line - 1, note.col - 1),
                      Position(note.line - 1, note.col - 1 + length))

    Diagnostic(range,
               Some(severity),
               code = None,
               source = Some("Scala"),
               message = note.msg)
  }
}

class EnsimeLanguageServer(in: InputStream, out: OutputStream)
    extends LanguageServer(in, out) {
  private var system: ActorSystem      = _
  private var fileStore: TempFileStore = _
  private var projectPath: String      = _
  // these fields will eventually become constructor params

  // Ensime root actor
  private var ensimeActor: ActorRef = _
  implicit val timeout: Timeout     = Timeout(5 seconds)

  override def start(): Unit = {
    super.start()
    // if we got here it means the connection was closed
    // cleanup and exit
    shutdown()
  }

  override def initialize(
    pid: Long,
    rootPath: String,
    capabilities: ClientCapabilities
  ): ServerCapabilities = {
    log.info(s"Initialized with $pid, $rootPath, $capabilities")

    val rootFile = new File(rootPath)
    val cacheDir = new File(rootFile, ".ensime_cache")
    cacheDir.mkdir()

    initializeEnsime(rootPath)

    ServerCapabilities(
      completionProvider = Some(CompletionOptions(false, Seq("."))),
      definitionProvider = true,
      hoverProvider = true,
      documentSymbolProvider = true
    )
  }

  def loadConfig(ensimeFile: File): Config = {
    val config   = s"""ensime.config = "${ensimeFile.toString}" """
    val fallback = ConfigFactory.parseString(config)
    ConfigFactory.load().withFallback(fallback)
  }

  private def initializeEnsime(rootPath: String): Try[EnsimeConfig] = { // rewrite initialization
    val ensimeFile = new File(s"$rootPath/.ensime")

    val configT = Try {
      val config = loadConfig(ensimeFile)
      system = ActorSystem("ENSIME", config)
      val serverConfig: EnsimeServerConfig = parseServerConfig(config)
      val ensimeConfig = EnsimeConfigProtocol.parse(
        serverConfig.config.file.readString()(MessageReader.Utf8Charset)
      )
      (ensimeConfig, serverConfig)
    }

    configT match {
      case Failure(e) =>
        log.error(s"initializeEnsime Error: ${e.getMessage}")
        e.printStackTrace()

        if (ensimeFile.exists) {
          connection.showMessage(MessageType.Error,
                                 s"Error parsing .ensime: ${e.getMessage}")
        } else {
          connection.showMessage(
            MessageType.Error,
            s"No .ensime file in directory. Run `sbt ensimeConfig` to create one."
          )
        }
      case Success((config, serverConfig)) =>
        log.info(s"Using configuration: $config")
        val t = Try {
          fileStore = new TempFileStore(config.cacheDir.file.toString)
          ensimeActor = system.actorOf(
            Props(classOf[EnsimeActor], this, config, serverConfig),
            "server"
          )
          projectPath = rootPath
        }
        t.recover {
          case e =>
            log.error(s"initializeEnsime: ${e.getMessage}")
            e.printStackTrace()
            connection.showMessage(MessageType.Error,
                                   s"Error creating storage: ${e.getMessage}")
        }
        // we don't give a damn about them, but Ensime expects it
        ensimeActor ! ConnectionInfoReq
    }
    configT.map(_._1)
  }

  override def onChangeWatchedFiles(changes: Seq[FileEvent]): Unit =
    changes match {
      case FileEvent(uri, FileChangeType.Created | FileChangeType.Changed) +: _ =>
        val rootPath =
          Try(new URI(uri).toURL.getPath).filter(_ == s"$projectPath/.ensime")
        rootPath.foreach { path =>
          connection.showMessage(MessageType.Info,
                                 ".ensime file change detected. Reloading")
          if (ensimeActor ne null)
            ensimeActor ! ShutdownRequest(".ensime file changed")
          initializeEnsime(path)
        }

      case _ => ()
    }

  override def onOpenTextDocument(td: TextDocumentItem): Unit =
    if (!(ensimeActor eq null)) {
      val uri = new URI(td.uri)
      if (uri.getScheme == "file") {
        val f = new File(uri)
        if (f.getAbsolutePath.startsWith(fileStore.path)) {
          log.debug(s"Not adding temporary file $f to Ensime")
        } else {
          ensimeActor ! TypecheckFileReq(
            SourceFileInfo(RawFile(f.toPath), Some(td.text))
          )
        }
      } else {
        log.info(s"Non-file URI in openTextDocument: ${td.uri}")
      }
    }

  override def onChangeTextDocument(
    td: VersionedTextDocumentIdentifier,
    changes: Seq[TextDocumentContentChangeEvent]
  ): Unit = {
    // we assume full text sync
    assert(changes.size == 1)
    val change = changes.head
    assert(change.range.isEmpty)
    assert(change.rangeLength.isEmpty)

    ensimeActor ! TypecheckFileReq(
      EnsimeLanguageServer.toSourceFileInfo(td.uri, Some(change.text))
    )
  }

  override def onSaveTextDocument(td: TextDocumentIdentifier): Unit =
    log.debug(s"saveTextDocument $td")

  override def onCloseTextDocument(td: TextDocumentIdentifier): Unit = {
    log.debug(s"Removing ${td.uri} from Ensime.")
    val doc = documentManager.documentForUri(td.uri)
    doc.foreach(d => ensimeActor ! RemoveFileReq(d.toFile))
  }

  def publishDiagnostics(diagnostics: List[Note]): Unit = {
    val byFile = diagnostics.groupBy(_.file)

    log.info(s"Received ${diagnostics.size} notes.")

    for {
      doc  <- documentManager.allOpenDocuments
      path = Paths.get(new URI(doc.uri)).toString
    } connection.publishDiagnostics(
      doc.uri,
      byFile.getOrElse(path, List.empty).map(EnsimeLanguageServer.toDiagnostic)
    )
  }

  override def shutdown(): Unit = {
    log.info("Shutdown request")
    system.terminate()
    log.info("Shutting down actor system.")
    Await.result(system.whenTerminated, Duration.Inf)
    log.info("Actor system down.")
  }

  override def completionRequest(textDocument: TextDocumentIdentifier,
                                 position: Position): CompletionList = {
    import scala.concurrent.ExecutionContext.Implicits._

    val res = for (doc <- documentManager.documentForUri(textDocument.uri))
      yield {
        val future = ensimeActor ? CompletionsReq(
          EnsimeLanguageServer.toSourceFileInfo(textDocument.uri,
                                                Some(new String(doc.contents))),
          doc.positionToOffset(position),
          100,
          caseSens = false,
          reload = false
        )

        future.onComplete(
          f =>
            log.debug(s"Completions future completed: success? ${f.isSuccess}")
        )

        future.map {
          case CompletionInfoList(prefix, completions) =>
            log.debug(
              s"Received ${completions.size} completions: ${completions.take(10).map(_.name)}"
            )
            completions
              .sortBy(-_.relevance)
              .map(EnsimeLanguageServer.toCompletion)
        }
      }

    res
      .map(f => CompletionList(false, Await.result(f, 5 seconds)))
      .getOrElse(CompletionList(false, Nil))
  }

  override def gotoDefinitionRequest(textDocument: TextDocumentIdentifier,
                                     position: Position): DefinitionResult = {
    import scala.concurrent.ExecutionContext.Implicits._
    log.info(
      s"Got goto definition request at (${position.line}, ${position.character})."
    )

    val res = for (doc <- documentManager.documentForUri(textDocument.uri))
      yield {
        val future = ensimeActor ? SymbolAtPointReq(
          Right(
            EnsimeLanguageServer.toSourceFileInfo(
              textDocument.uri,
              Some(new String(doc.contents))
            )
          ),
          doc.positionToOffset(position)
        )

        future.onComplete(
          f =>
            log.debug(
              s"Goto Definition future completed: succes? ${f.isSuccess}"
          )
        )

        future.map {
          case SymbolInfo(name, localName, declPos, typeInfo) =>
            declPos.toSeq.flatMap {
              case OffsetSourcePosition(ensimeFile, offset) =>
                fileStore
                  .getFile(ensimeFile)
                  .map(path => {
                    val file = path.toFile
                    val uri  = file.toURI.toString
                    val doc = TextDocument(
                      uri,
                      file.readString()(MessageReader.Utf8Charset).toCharArray
                    )
                    val start = doc.offsetToPosition(offset)
                    val end = start.copy(
                      character = start.character + localName.length()
                    )

                    log.info(s"Found definition at $uri, line: ${start.line}")
                    Seq(Location(uri, Range(start, end)))
                  })
                  .recover {
                    case e =>
                      log.error(s"Couldn't retrieve hyperlink target file $e")
                      Seq()
                  }
                  .get

              case _ =>
                Seq()
            }
        }
      }

    val locs = res.map(f => Await.result(f, 5 seconds)).getOrElse(Vector.empty)
    DefinitionResult(locs)
  }

  override def hoverRequest(textDocument: TextDocumentIdentifier,
                            position: Position): Hover = {
    import scala.concurrent.ExecutionContext.Implicits._
    log.info(s"Got hover request at (${position.line}, ${position.character}).")

    val res = for (doc <- documentManager.documentForUri(textDocument.uri))
      yield {
        val future = ensimeActor ? DocUriAtPointReq(
          Right(
            EnsimeLanguageServer.toSourceFileInfo(
              textDocument.uri,
              Some(new String(doc.contents))
            )
          ),
          OffsetRange(doc.positionToOffset(position))
        )

        future.onComplete(
          f =>
            log.debug(
              s"DocUriAtPointReq future completed: succes? ${f.isSuccess}"
          )
        )

        future.map {
          case Some(DocSigPair(DocSig(_, scalaSig), DocSig(_, javaSig))) =>
            val sig = scalaSig.orElse(javaSig).getOrElse("")
            log.info(s"Retrieved signature $sig from @sigPair")
            Hover(Seq(RawMarkedString("scala", sig)),
                  Some(Range(position, position)))
          case None =>
            log.info(s"No signature")
            Hover(Seq.empty, None)
        }
      }
    res.map(f => Await.result(f, 5 seconds)).getOrElse(Hover(Nil, None))
  }

  override def documentSymbols(
    tdi: TextDocumentIdentifier
  ): Seq[SymbolInformation] = {
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.concurrent.Future

    if (ensimeActor ne null) {
      val res: Option[Future[List[SymbolInformation]]] =
        for (doc <- documentManager.documentForUri(tdi.uri)) yield {

          def toSymbolInformation(
            structure: StructureViewMember,
            outer: Option[String]
          ): Seq[SymbolInformation] =
            structure match {
              case StructureViewMember(keyword, name, pos, members) =>
                val kind = EnsimeLanguageServer.KeywordToKind
                  .getOrElse(keyword, SymbolKind.Field)
                val rest =
                  members.flatMap(m => toSymbolInformation(m, Some(name)))
                val position = pos match {
                  case OffsetSourcePosition(_, offset) =>
                    doc.offsetToPosition(offset)
                  case LineSourcePosition(_, line) => Position(line, 0)
                  case _ =>
                    log.error(s"Unknown position for $name: $pos")
                    Position(0, 0)
                }

                SymbolInformation(
                  name,
                  kind,
                  Location(tdi.uri,
                           Range(position,
                                 position.copy(
                                   character = position.character + name.length
                                 ))),
                  outer
                ) +: rest

              case _ =>
                log.error(s"Unknown structure element: $structure")
                Seq.empty
            }

          log.info(s"Document Symbols request for ${tdi.uri}")
          val future = ensimeActor ? StructureViewReq(
            EnsimeLanguageServer
              .toSourceFileInfo(tdi.uri, Some(new String(doc.contents)))
          )

          future.onComplete(
            f =>
              log.debug(
                s"StructureView future completed: succes? ${f.isSuccess}"
            )
          )

          future.map {
            case StructureView(members) =>
              log.debug(s"got back: $members")
              members.flatMap(m => toSymbolInformation(m, None))
          }
        }
      res.map(f => Await.result(f, 5 seconds)).getOrElse(Seq.empty)
    } else {
      Seq.empty
    }
  }
}
