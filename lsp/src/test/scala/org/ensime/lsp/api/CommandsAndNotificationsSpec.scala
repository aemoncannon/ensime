// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.api

import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.companions._
import org.ensime.lsp.api.types._
import org.ensime.lsp.rpc.companions._
import org.ensime.lsp.rpc.messages._
import org.scalatest.Matchers._
import org.scalatest._
import spray.json._

class CommandsAndNotificationsSpec extends FreeSpec {

  def serverCommandShouldReadAndWrite[T <: ServerCommand: RpcCommand](
    obj: T,
    id: CorrelationId,
    message: JsonRpcRequestMessage
  ): Unit = {
    s"should correctly write $obj" in {
      ServerCommand.write(obj, id) shouldEqual message
    }

    s"should correctly read $message" in {
      ServerCommand.read(message) shouldEqual Right(obj)
    }
  }

  def notificationShouldReadAndWrite[T <: Notification: RpcNotification](
    obj: T,
    message: JsonRpcNotificationMessage
  ): Unit = {
    s"should correctly write $obj" in {
      Notification.write(obj) shouldEqual message
    }

    s"should correctly read $message" in {
      Notification.read(message) shouldEqual Right(obj)
    }
  }

  def parseJsObject(json: String): JsObject =
    JsParser(json) match {
      case obj: JsObject => obj
      case _             => throw new IllegalArgumentException("should be a json object")
    }

  "initialize command" - {
    val initializeParams = InitializeParams(42L, "root", ClientCapabilities())
    val id               = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "initialize",
      Params(
        parseJsObject("""
                        |{
                        |  "processId": 42,
                        |  "rootPath": "root",
                        |  "capabilities": {}
                        |}""".stripMargin)
      ),
      NumberId(1)
    )

    import ServerCommands._
    serverCommandShouldReadAndWrite(initializeParams, id, message)
  }

  "shutdown command" - {
    val shutdown = Shutdown()
    val id       = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "shutdown",
      Params(JsObject.empty),
      NumberId(1)
    )
    val messageWithNull = JsonRpcRequestMessage(
      "shutdown",
      None,
      NumberId(1)
    )

    import ServerCommands._
    serverCommandShouldReadAndWrite(shutdown, id, message)

    s"should correctly read $messageWithNull" in {
      ServerCommand.read(messageWithNull) shouldEqual Right(shutdown)
    }
  }

  "completion command" - {
    val completionRequest = TextDocumentCompletionRequest(
      TextDocumentPositionParams(TextDocumentIdentifier("uri1"),
                                 Position(10, 20))
    )
    val id = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "textDocument/completion",
      Params(
        parseJsObject("""
                        |{
                        |  "textDocument": {
                        |    "uri": "uri1"
                        |  },
                        |  "position": {
                        |    "line": 10,
                        |    "character": 20
                        |  }
                        |}""".stripMargin)
      ),
      NumberId(1)
    )

    import ServerCommands._
    serverCommandShouldReadAndWrite(completionRequest, id, message)
  }

  "definition command" - {
    val definitionRequest = TextDocumentDefinitionRequest(
      TextDocumentPositionParams(TextDocumentIdentifier("uri1"),
                                 Position(10, 20))
    )
    val id = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "textDocument/definition",
      Params(
        parseJsObject("""
                        |{
                        |  "textDocument": {
                        |    "uri": "uri1"
                        |  },
                        |  "position": {
                        |    "line": 10,
                        |    "character": 20
                        |  }
                        |}""".stripMargin)
      ),
      NumberId(1)
    )

    import ServerCommands._
    serverCommandShouldReadAndWrite(definitionRequest, id, message)
  }

  "hover command" - {
    val hoverRequest = TextDocumentHoverRequest(
      TextDocumentPositionParams(TextDocumentIdentifier("uri1"),
                                 Position(10, 20))
    )
    val id = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "textDocument/hover",
      Params(
        parseJsObject("""
                        |{
                        |  "textDocument": {
                        |    "uri": "uri1"
                        |  },
                        |  "position": {
                        |    "line": 10,
                        |    "character": 20
                        |  }
                        |}""".stripMargin)
      ),
      NumberId(1)
    )

    import ServerCommands._
    serverCommandShouldReadAndWrite(hoverRequest, id, message)
  }

  "documentSymbol command" - {
    val documentSymbolParams =
      DocumentSymbolParams(TextDocumentIdentifier("uri1"))
    val id = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "textDocument/documentSymbol",
      Params(
        parseJsObject("""
                        |{
                        |  "textDocument": {
                        |    "uri": "uri1"
                        |  }
                        |}""".stripMargin)
      ),
      NumberId(1)
    )

    import ServerCommands._
    serverCommandShouldReadAndWrite(documentSymbolParams, id, message)
  }

  "showMessageRequest command" - {
    val requestParams =
      ShowMessageRequestParams(
        5L,
        "message1",
        Seq(MessageActionItem("title1"), MessageActionItem("title2"))
      )
    val id = CorrelationId(1)
    val message = JsonRpcRequestMessage(
      "showMessageRequest",
      Params(
        parseJsObject("""
                        |{
                        |  "tpe": 5,
                        |  "message": "message1",
                        |  "actions": [{"title": "title1"},{"title": "title2"}]
                        |}""".stripMargin)
      ),
      NumberId(1)
    )

    import ClientCommands._
    s"should correctly write $requestParams" in {
      ClientCommand.write(requestParams, id) shouldEqual message
    }

    s"should correctly read $message" in {
      ClientCommand.read(message) shouldEqual Right(requestParams)
    }
  }

  "showMessage notification" - {
    val showMessageParams = ShowMessageParams(5, "message1")
    val message = JsonRpcNotificationMessage(
      "window/showMessage",
      Params(
        parseJsObject("""{"tpe": 5, "message": "message1"}""")
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(showMessageParams, message)
  }

  "logMessage notification" - {
    val logMessageParams = LogMessageParams(5, "message1")
    val message = JsonRpcNotificationMessage(
      "window/logMessage",
      Params(
        parseJsObject("""{"tpe": 5, "message": "message1"}""")
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(logMessageParams, message)
  }

  "publishDiagnostics notification" - {
    val publishDiagnostics =
      PublishDiagnostics(
        "uri1",
        Seq(
          Diagnostic(
            Range(Position(1, 5), Position(2, 4)),
            Some(2),
            None, // Official protocol definition says Option fields can be omitted
            Some("some text"),
            "message1"
          )
        )
      )
    val message = JsonRpcNotificationMessage(
      "textDocument/publishDiagnostics",
      Params(
        parseJsObject("""
                        |{
                        |  "uri": "uri1",
                        |  "diagnostics": [{
                        |     "range": {
                        |      "start": {
                        |        "line": 1,
                        |        "character": 5
                        |      },
                        |      "end": {
                        |        "line": 2,
                        |        "character": 4
                        |      }
                        |    },
                        |    "severity": 2,
                        |    "source":"some text",
                        |    "message":"message1"
                        |  }]
                        |}""".stripMargin)
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(publishDiagnostics, message)
  }

  "didOpen notification" - {
    val didOpenTextDocumentParams =
      DidOpenTextDocumentParams(TextDocumentItem("uri1", "en", 1L, "some text"))
    val message = JsonRpcNotificationMessage(
      "textDocument/didOpen",
      Params(
        parseJsObject("""
                        |{
                        |  "textDocument":{
                        |    "uri": "uri1",
                        |    "languageId": "en",
                        |    "version": 1,
                        |    "text": "some text"
                        |  }
                        |}""".stripMargin)
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(didOpenTextDocumentParams, message)
  }

  "didOpen notification" - {
    val didChangeTextDocumentParams =
      DidChangeTextDocumentParams(
        VersionedTextDocumentIdentifier("uri1", 2L),
        Seq(TextDocumentContentChangeEvent(None, Some(10), "some text"))
      )
    val message = JsonRpcNotificationMessage(
      "textDocument/didChange",
      Params(
        parseJsObject("""
                        |{
                        |  "textDocument": {
                        |    "uri": "uri1",
                        |    "version": 2
                        |  },
                        |  "contentChanges": [{
                        |    "rangeLength": 10,
                        |    "text": "some text"
                        |  }]
                        |}""".stripMargin)
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(didChangeTextDocumentParams, message)
  }

  "didClose notification" - {
    val didCloseTextDocumentParams =
      DidCloseTextDocumentParams(TextDocumentIdentifier("uri1"))
    val message = JsonRpcNotificationMessage(
      "textDocument/didClose",
      Params(
        parseJsObject("""{"textDocument": {"uri": "uri1"}}""")
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(didCloseTextDocumentParams, message)
  }

  "didSave notification" - {
    val didCloseTextDocumentParams =
      DidSaveTextDocumentParams(TextDocumentIdentifier("uri1"))
    val message = JsonRpcNotificationMessage(
      "textDocument/didSave",
      Params(
        parseJsObject("""{"textDocument": {"uri": "uri1"}}""")
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(didCloseTextDocumentParams, message)
  }

  "didChangeWatchedFiles notification" - {
    val didChangeWatchedFiles =
      DidChangeWatchedFiles(Seq(FileEvent("uri1", 3), FileEvent("uri2", 4)))
    val message = JsonRpcNotificationMessage(
      "workspace/didChangeWatchedFiles",
      Params(
        parseJsObject("""|{
                         |  "changes": [
                         |    {"uri": "uri1", "type": 3},
                         |    {"uri": "uri2", "type": 4}
                         |  ]
                         |}""".stripMargin)
      )
    )

    import Notifications._
    notificationShouldReadAndWrite(didChangeWatchedFiles, message)
  }

  "initialized notification" - {
    val initialized = Initialized()
    val message = JsonRpcNotificationMessage(
      "initialized",
      Params(JsObject.empty)
    )

    import Notifications._
    notificationShouldReadAndWrite(initialized, message)
  }

  "cancelRequest notification" - {
    val cancelRequest = CancelRequest(3)
    val message = JsonRpcNotificationMessage(
      "$/cancelRequest",
      Params(parseJsObject("""{"id": 3}"""))
    )

    import Notifications._
    notificationShouldReadAndWrite(cancelRequest, message)
  }
}
