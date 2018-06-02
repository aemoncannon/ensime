// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.rpc

import org.ensime.lsp.rpc.messages._
import org.scalactic.source.Position
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import spray.json._

import scala.collection.immutable.Seq

class JsonRpcMessageSpec extends FreeSpec {

  import JsWriter.ops._
  import JsReader.ops._
  implicit class JsParserOps(s: String) {
    def parseJson: JsValue = JsParser(s)
  }

  def willFailToDecode[T: JsReader: JsWriter](
    json: JsValue
  )(implicit p: Position): Unit =
    s"will fail to decode" in {
      json.as[T] should be('left)
    }

  def willDecodeAndEncode[T: JsReader: JsWriter](message: T, json: JsValue)(
    implicit p: Position
  ): Unit = {
    s"will decode to $message" in {
      json.as[T] shouldEqual Right(message)
    }
    s"will encode to $json" in {
      message.toJson shouldEqual json
    }
  }

  "An invalid JsValue" - {
    val json = "{}".parseJson
    willFailToDecode[JsonRpcMessage](json)
  }

  "A JsonRpcRequestMessage" - {
    "with an incorrect version" - {
      val json =
        """
          |{
          |  "jsonrpc":"3.0",
          |  "method":"testMethod",
          |  "params":{"param1":"param1","param2":"param2"},
          |  "id":0
          |}""".stripMargin.parseJson

      willFailToDecode[JsonRpcRequestMessage](json)
    }
    "with version of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":2.0,
                   |  "method":"testMethod",
                   |  "params":{"param1":"param1","param2":"param2"},
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessage](json)
    }
    "without a version" - {
      val json = """
                   |{
                   |  "method":"testMethod",
                   |  "params":{"param1":"param1","param2":"param2"},
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessage](json)
    }
    "with method of the wrong type" - {
      val json =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "method":3.0,
          |  "params":{"param1":"param1","param2":"param2"},
          |  "id":0
          |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessage](json)
    }
    "without a method" - {
      val json =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "params":{"param1":"param1","param2":"param2"},
          |  "id":0
          |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessage](json)
    }
    "with params of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":"2.0",
                   |  "method":"testMethod",
                   |  "params":"params",
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessage](json)
    }
    "without params" - {
      val jsonRpcRequestMessage = JsonRpcRequestMessage(
        "testMethod",
        Params(),
        CorrelationId(1)
      )
      val jsonRpcRequestMessageJson = """
                                        |{
                                        |  "jsonrpc":"2.0",
                                        |  "method":"testMethod",
                                        |  "id":1
                                        |}""".stripMargin.parseJson

      willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)
    }
    "with a params array" - {
      "and a string id" - {
        val jsonRpcRequestMessage = JsonRpcRequestMessage(
          method = "testMethod",
          Params(
            JsArray(
              JsString("param1"),
              JsString("param2")
            )
          ),
          CorrelationId("one")
        )
        val jsonRpcRequestMessageJson = """
                                          |{
                                          |  "jsonrpc":"2.0",
                                          |  "method":"testMethod",
                                          |  "params":["param1","param2"],
                                          |  "id":"one"
                                          |}""".stripMargin.parseJson
        willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)
      }
      "and a numeric id" - {
        val jsonRpcRequestMessage = JsonRpcRequestMessage(
          method = "testMethod",
          Params(
            JsArray(
              JsString("param1"),
              JsString("param2")
            )
          ),
          CorrelationId(1)
        )
        val jsonRpcRequestMessageJson =
          """
            |{
            |  "jsonrpc":"2.0",
            |  "method":"testMethod",
            |  "params":["param1","param2"],
            |  "id":1
            |}""".stripMargin.parseJson
        willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)

        "with a fractional part" - {
          val jsonRpcRequestMessage = JsonRpcRequestMessage(
            method = "testMethod",
            Params(
              JsArray(
                JsString("param1"),
                JsString("param2")
              )
            ),
            CorrelationId(1.1)
          )
          val jsonRpcRequestMessageJson = """{
                                            |  "jsonrpc":"2.0",
                                            |  "method":"testMethod",
                                            |  "params":["param1","param2"],
                                            |  "id":1.1
                                            |}""".stripMargin.parseJson
          willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)
        }
      }
    }
    "with a params object" - {
      "and a string id" - {
        val jsonRpcRequestMessage = JsonRpcRequestMessage(
          method = "testMethod",
          Params(
            JsObject(
              "param1" -> JsString("param1"),
              "param2" -> JsString("param2")
            )
          ),
          CorrelationId("one")
        )
        val jsonRpcRequestMessageJson =
          """
            |{
            |  "jsonrpc":"2.0",
            |  "method":"testMethod",
            |  "params":{"param1":"param1","param2":"param2"},
            |  "id":"one"
            |}""".stripMargin.parseJson
        willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)
      }
      "and a numeric id" - {
        val jsonRpcRequestMessage = JsonRpcRequestMessage(
          method = "testMethod",
          Params(
            JsObject(
              "param1" -> JsString("param1"),
              "param2" -> JsString("param2")
            )
          ),
          CorrelationId(1)
        )
        val jsonRpcRequestMessageJson =
          """
            |{
            |  "jsonrpc":"2.0",
            |  "method":"testMethod",
            |  "params":{"param1":"param1","param2":"param2"},
            |  "id":1
            |}""".stripMargin.parseJson
        willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)

        "with a fractional part" - {
          val jsonRpcRequestMessage = JsonRpcRequestMessage(
            method = "testMethod",
            Params(
              JsObject(
                "param1" -> JsString("param1"),
                "param2" -> JsString("param2")
              )
            ),
            CorrelationId(1.1)
          )
          val jsonRpcRequestMessageJson =
            """
              |{
              |  "jsonrpc":"2.0",
              |  "method":"testMethod",
              |  "params":{"param1":"param1","param2":"param2"},
              |  "id":1.1
              |}""".stripMargin.parseJson
          willDecodeAndEncode(jsonRpcRequestMessage, jsonRpcRequestMessageJson)
        }
      }
    }
  }

  "A JsonRpcRequestMessageBatch" - {
    "with no content" - {
      val json = """
                   |[
                   |]""".stripMargin.parseJson

      willFailToDecode[JsonRpcRequestMessageBatch](json)
    }
    "with an invalid request" - {
      val json = """
                   |[
                   |  {
                   |    "jsonrpc":"2.0",
                   |    "params":{"param1":"param1","param2":"param2"},
                   |    "id":1
                   |  }
                   |]""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessageBatch](json)
    }
    "with a single request" - {
      val jsonRpcRequestMessageBatch = JsonRpcRequestMessageBatch(
        Seq(
          JsonRpcRequestMessage(
            method = "testMethod",
            Params(
              JsObject(
                "param1" -> JsString("param1"),
                "param2" -> JsString("param2")
              )
            ),
            CorrelationId(1)
          )
        )
      )
      val jsonRpcRequestMessageBatchJson =
        """[
          |  {
          |    "jsonrpc":"2.0",
          |    "method":"testMethod",
          |    "params":{"param1":"param1","param2":"param2"},
          |    "id":1
          |  }
          |]""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcRequestMessageBatch,
                          jsonRpcRequestMessageBatchJson)
    }
    "with an invalid notification" - {
      val json = """
                   |[
                   |  {
                   |    "jsonrpc":"2.0"
                   |  }
                   |]""".stripMargin.parseJson
      willFailToDecode[JsonRpcRequestMessageBatch](json)
    }
    "with a single notification" - {
      val jsonRpcRequestMessageBatch = JsonRpcRequestMessageBatch(
        Seq(
          JsonRpcNotificationMessage(
            method = "testMethod",
            Params(
              JsObject(
                "param1" -> JsString("param1"),
                "param2" -> JsString("param2")
              )
            )
          )
        )
      )
      val jsonRpcRequestMessageBatchJson =
        """
          |[
          |  {
          |    "jsonrpc":"2.0",
          |    "method":"testMethod",
          |    "params":{"param1":"param1","param2":"param2"}
          |  }
          |]""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcRequestMessageBatch,
                          jsonRpcRequestMessageBatchJson)
    }
  }

  "A JsonRpcResponseMessage" - {
    "with an incorrect version" - {
      val json = """
                   |{
                   |  "jsonrpc":"3.0",
                   |  "result":{"param1":"param1","param2":"param2"},
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcResponseMessage](json)
    }
    "with version of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":2.0,
                   |  "result":{"param1":"param1","param2":"param2"},
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcResponseMessage](json)
    }
    "without a version" - {
      val json = """
                   |{
                   |  "result":{"param1":"param1","param2":"param2"},
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcResponseMessage](json)
    }
    "with an error of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":"2.0",
                   |  "error":"error",
                   |  "id":0
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcResponseMessage](json)
    }
    "with a parse error" - {
      val jsonRpcResponseMessage = JsonRpcResponseErrorMessages.parseError(
        new Throwable("Boom"),
        CorrelationId(1)
      )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-32700,"message":"Parse error","data":{"meaning":"Invalid JSON was received by the server.\nAn error occurred on the server while parsing the JSON text.","error":"Boom"}},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with an invalid request error" - {
      val jsonRpcResponseMessage = JsonRpcResponseErrorMessages.invalidRequest(
        DeserializationException("Boom"),
        CorrelationId(1)
      )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-32600,"message":"Invalid Request","data":{"meaning":"The JSON sent is not a valid Request object.","error":"Boom"}},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with a method not found error" - {
      val jsonRpcResponseMessage = JsonRpcResponseErrorMessages.methodNotFound(
        "foo",
        CorrelationId(1)
      )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-32601,"message":"Method not found","data":{"meaning":"The method does not exist / is not available.","error":"The method \"foo\" is not implemented."}},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with an invalid params error" - {
      val jsonRpcResponseMessage = JsonRpcResponseErrorMessages.invalidParams(
        "Boom",
        CorrelationId(1)
      )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-32602,"message":"Invalid params","data":{"meaning":"Invalid method parameter(s).","error":"Boom"}},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with an internal error" - {
      val jsonRpcResponseMessage = JsonRpcResponseErrorMessages.internalError(
        error = None,
        CorrelationId(1)
      )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-32603,"message":"Internal error","data":{"meaning":"Internal JSON-RPC error."}},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with a server error" - {
      val jsonRpcResponseMessage = JsonRpcResponseErrorMessages.serverError(
        code = JsonRpcResponseErrorMessages.ServerErrorCodeFloor,
        error = None,
        CorrelationId(1)
      )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-32099,"message":"Server error","data":{"meaning":"Something went wrong in the receiving application."}},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with an application error" - {
      val jsonRpcResponseMessage =
        JsonRpcResponseErrorMessages.applicationError(
          code = -31999,
          message = "Boom",
          data = None,
          CorrelationId(1)
        )
      val jsonRpcResponseMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "error":{"code":-31999,"message":"Boom"},
          |  "id":1
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
    }
    "with a result" - {
      "and a string id" - {
        val jsonRpcResponseMessage = JsonRpcResponseSuccessMessage(
          JsObject(
            "param1" -> JsString("param1"),
            "param2" -> JsString("param2")
          ),
          CorrelationId("one")
        )
        val jsonRpcResponseMessageJson =
          """
            |{
            |  "jsonrpc":"2.0",
            |  "result":{"param1":"param1","param2":"param2"},
            |  "id":"one"
            |}""".stripMargin.parseJson
        willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)
      }
      "and a numeric id" - {
        val jsonRpcResponseMessage = JsonRpcResponseSuccessMessage(
          JsObject(
            "param1" -> JsString("param1"),
            "param2" -> JsString("param2")
          ),
          CorrelationId(1)
        )
        val jsonRpcResponseMessageJson =
          """
            |{
            |  "jsonrpc":"2.0",
            |  "result":{"param1":"param1","param2":"param2"},
            |  "id":1
            |}""".stripMargin.parseJson
        willDecodeAndEncode(jsonRpcResponseMessage, jsonRpcResponseMessageJson)

        "with a fractional part" - {
          val jsonRpcResponseMessage = JsonRpcResponseSuccessMessage(
            JsObject(
              "param1" -> JsString("param1"),
              "param2" -> JsString("param2")
            ),
            CorrelationId(1.1)
          )
          val jsonRpcResponseMessageJson =
            """
              |{
              |  "jsonrpc":"2.0",
              |  "result":{"param1":"param1","param2":"param2"},
              |  "id":1.1
              |}""".stripMargin.parseJson
          willDecodeAndEncode(jsonRpcResponseMessage,
                              jsonRpcResponseMessageJson)
        }
      }
    }
  }

  "A JsonRpcResponseMessageBatch" - {
    "with no content" - {
      val json = """
                   |[
                   |]""".stripMargin.parseJson
      willFailToDecode[JsonRpcResponseMessageBatch](json)
    }
    "with a single response" - {
      val jsonRpcResponseMessageBatch = JsonRpcResponseMessageBatch(
        Seq(
          JsonRpcResponseSuccessMessage(
            JsObject(
              "param1" -> JsString("param1"),
              "param2" -> JsString("param2")
            ),
            CorrelationId(1)
          )
        )
      )
      val jsonRpcResponseMessageBatchJson =
        """
          |[
          |  {
          |    "jsonrpc":"2.0",
          |    "result":{"param1":"param1","param2":"param2"},
          |    "id":1
          |  }
          |]""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcResponseMessageBatch,
                          jsonRpcResponseMessageBatchJson)
    }
  }

  "A JsonRpcNotificationMessage" - {
    "with an incorrect version" - {
      val json = """
                   |{
                   |  "jsonrpc":"3.0",
                   |  "method":"testMethod",
                   |  "params":{"param1":"param1","param2":"param2"}
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcNotificationMessage](json)
    }
    "with version of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":2.0,
                   |  "method":"testMethod",
                   |  "params":{"param1":"param1","param2":"param2"}
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcNotificationMessage](json)
    }
    "without a version" - {
      val json = """
                   |{
                   |  "method":"testMethod",
                   |  "params":{"param1":"param1","param2":"param2"}
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcNotificationMessage](json)
    }
    "with method of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":"2.0",
                   |  "method":3.0,
                   |  "params":{"param1":"param1","param2":"param2"}
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcNotificationMessage](json)
    }
    "without a method" - {
      val json = """
                   |{
                   |  "jsonrpc":"2.0",
                   |  "params":{"param1":"param1","param2":"param2"}
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcNotificationMessage](json)
    }
    "with params of the wrong type" - {
      val json = """
                   |{
                   |  "jsonrpc":"2.0",
                   |  "method":"testMethod",
                   |  "params":"params"
                   |}""".stripMargin.parseJson
      willFailToDecode[JsonRpcNotificationMessage](json)
    }
    "without params" - {
      val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
        method = "testMethod",
        Params()
      )
      val jsonRpcNotificationMessageJson = """
                                             |{
                                             |  "jsonrpc":"2.0",
                                             |  "method":"testMethod"
                                             |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcNotificationMessage,
                          jsonRpcNotificationMessageJson)
    }
    "with a params array" - {
      val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
        method = "testMethod",
        Params(
          JsArray(
            JsString("param1"),
            JsString("param2")
          )
        )
      )
      val jsonRpcNotificationMessageJson = """
                                             |{
                                             |  "jsonrpc":"2.0",
                                             |  "method":"testMethod",
                                             |  "params":["param1","param2"]
                                             |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcNotificationMessage,
                          jsonRpcNotificationMessageJson)
    }
    "with a params object" - {
      val jsonRpcNotificationMessage = JsonRpcNotificationMessage(
        method = "testMethod",
        Params(
          JsObject(
            "param1" -> JsString("param1"),
            "param2" -> JsString("param2")
          )
        )
      )
      val jsonRpcNotificationMessageJson =
        """
          |{
          |  "jsonrpc":"2.0",
          |  "method":"testMethod",
          |  "params":{"param1":"param1","param2":"param2"}
          |}""".stripMargin.parseJson
      willDecodeAndEncode(jsonRpcNotificationMessage,
                          jsonRpcNotificationMessageJson)
    }
  }
}
