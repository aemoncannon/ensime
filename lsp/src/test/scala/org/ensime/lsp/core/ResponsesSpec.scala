// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import org.ensime.lsp.api.commands._
import org.ensime.lsp.api.types._
import org.ensime.lsp.rpc.companions.RpcResponse
import org.ensime.lsp.rpc.messages.{
  CorrelationId,
  JsonRpcResponseSuccessMessage
}
import org.scalatest.Matchers._
import org.scalatest._
import shapeless.Typeable
import spray.json._

class ResponsesSpec extends FreeSpec {
  def responseShouldReadAndWrite[T: JsReader: JsWriter: Typeable](
    obj: T,
    id: CorrelationId,
    message: JsonRpcResponseSuccessMessage
  ): Unit = {
    s"should correctly write $obj" in {
      RpcResponse.write(obj, id) shouldEqual message
    }

    s"should correctly read $message to ${Typeable[T].describe}" in {
      RpcResponse.read[T](message) shouldEqual Right(obj)
    }
  }

  "InitializeResult response" - {
    val initializeResult = InitializeResult(
      ServerCapabilities(
        completionProvider = Some(CompletionOptions(true, Seq("a", "b", "c")))
      )
    )
    val id = CorrelationId(1)
    val message = JsonRpcResponseSuccessMessage(
      JsParser(
        """
          |{
          |  "capabilities": {
          |    "definitionProvider": false,
          |    "hoverProvider": false,
          |    "workspaceSymbolProvider": false,
          |    "renameProvider": false,
          |    "referencesProvider": false,
          |    "completionProvider": {
          |      "resolveProvider": true,
          |      "triggerCharacters": ["a", "b", "c"]
          |    },
          |    "documentRangeFormattingProvider": false,
          |    "documentHighlightProvider": false,
          |    "textDocumentSync": 1,
          |    "codeActionProvider": false,
          |    "documentSymbolProvider": false,
          |    "documentFormattingProvider": false
          |  }
          |}""".stripMargin
      ),
      id
    )

    responseShouldReadAndWrite(initializeResult, id, message)
  }

  "CompletionList response" - {
    val completionList =
      CompletionList(true, Seq(CompletionItem(label = "label1")))
    val id = CorrelationId(1)
    val message = JsonRpcResponseSuccessMessage(
      JsParser("""
                 |{
                 |  "isIncomplete": true,
                 |  "items": [{"label": "label1"}]
                 |}""".stripMargin),
      id
    )

    responseShouldReadAndWrite(completionList, id, message)
  }

  "DefinitionResult response" - {
    val definitionResult = DefinitionResult(
      Seq(Location("uri1", Range(Position(1, 0), Position(2, 3))))
    )
    val id = CorrelationId(1)
    val message = JsonRpcResponseSuccessMessage(
      JsParser("""
                 |[{
                 |  "uri": "uri1",
                 |  "range": {
                 |    "start": {
                 |      "line": 1,
                 |      "character": 0
                 |    },
                 |    "end": {
                 |      "line": 2,
                 |      "character": 3
                 |    }
                 |  }
                 |}]""".stripMargin),
      id
    )

    responseShouldReadAndWrite(definitionResult, id, message)
  }

  "Hover response" - {
    val hover =
      Hover(
        Seq(RawMarkedString("lang1", "value1"), MarkdownString("some text")),
        None
      )
    val id = CorrelationId(1)
    val message = JsonRpcResponseSuccessMessage(
      JsParser("""
                 |{
                 |  "contents": [
                 |  {
                 |    "language": "lang1",
                 |    "value": "value1"
                 |  },
                 |  {
                 |    "contents": "some text"
                 |  }]
                 |}""".stripMargin),
      id
    )

    responseShouldReadAndWrite(hover, id, message)
  }

  "DocumentSymbolResult response" - {
    val documentSymbolResult =
      DocumentSymbolResult(
        Seq(
          SymbolInformation("name1",
                            2,
                            Location("uri1",
                                     Range(Position(1, 0), Position(4, 5))),
                            Some("container1"))
        )
      )
    val id = CorrelationId(1)
    val message = JsonRpcResponseSuccessMessage(
      JsParser("""
                 |[
                 |  {
                 |    "name": "name1",
                 |    "kind": 2,
                 |    "location": {
                 |      "uri": "uri1",
                 |      "range": {
                 |        "start": {
                 |          "line": 1,
                 |          "character": 0
                 |        },
                 |        "end": {
                 |          "line": 4,
                 |          "character": 5
                 |        }
                 |      }
                 |    },
                 |    "containerName": "container1"
                 |  }
                 |]""".stripMargin),
      id
    )

    responseShouldReadAndWrite(documentSymbolResult, id, message)
  }
}
