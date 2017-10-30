// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.rpc.messages

import spray.json._

import scala.collection.immutable.Seq

object JsonRpcMessages {
  final val Version = "2.0"
}

sealed trait CorrelationId
case object NullId                      extends CorrelationId
case class NumberId(number: BigDecimal) extends CorrelationId
case class StringId(str: String)        extends CorrelationId

object CorrelationId {
  def apply(): CorrelationId                   = NullId
  def apply(number: BigDecimal): CorrelationId = NumberId(number)
  def apply(str: String): CorrelationId        = StringId(str)
}

sealed trait Params
case object NullParams                 extends Params
case class ObjectParams(obj: JsObject) extends Params
case class ArrayParams(arr: JsArray)   extends Params

object Params {
  def apply(): Option[Params]              = None
  def apply(obj: JsObject): Option[Params] = Some(ObjectParams(obj))
  def apply(arr: JsArray): Option[Params]  = Some(ArrayParams(arr))
}

sealed abstract class JsonRpcMessage

sealed trait JsonRpcRequestOrNotificationMessage

final case class JsonRpcRequestMessage(jsonrpc: String,
                                       method: String,
                                       params: Option[Params],
                                       id: CorrelationId)
    extends JsonRpcMessage
    with JsonRpcRequestOrNotificationMessage {
  require(jsonrpc == JsonRpcMessages.Version)
}
object JsonRpcRequestMessage {
  def apply(method: String,
            params: Option[Params],
            id: CorrelationId): JsonRpcRequestMessage =
    apply(JsonRpcMessages.Version, method, params, id)
}

final case class JsonRpcNotificationMessage(jsonrpc: String,
                                            method: String,
                                            params: Option[Params])
    extends JsonRpcMessage
    with JsonRpcRequestOrNotificationMessage {
  require(jsonrpc == JsonRpcMessages.Version)
}
object JsonRpcNotificationMessage {
  def apply(method: String,
            params: Option[Params]): JsonRpcNotificationMessage =
    apply(JsonRpcMessages.Version, method, params)
}

final case class JsonRpcRequestMessageBatch(
  messages: Seq[JsonRpcRequestOrNotificationMessage]
) extends JsonRpcMessage {
  require(messages.nonEmpty)
}

sealed abstract class JsonRpcResponseMessage extends JsonRpcMessage {
  def id: CorrelationId
}

final case class JsonRpcResponseSuccessMessage(jsonrpc: String,
                                               result: JsValue,
                                               id: CorrelationId)
    extends JsonRpcResponseMessage {
  require(jsonrpc == JsonRpcMessages.Version)
}
object JsonRpcResponseSuccessMessage {
  def apply(result: JsValue, id: CorrelationId): JsonRpcResponseSuccessMessage =
    apply(JsonRpcMessages.Version, result, id)
}

final case class JsonRpcResponseErrorMessage(
  jsonrpc: String,
  error: JsonRpcResponseErrorMessage.Error,
  id: CorrelationId
) extends JsonRpcResponseMessage {
  require(jsonrpc == JsonRpcMessages.Version)
}
object JsonRpcResponseErrorMessage {
  case class Error(code: Int, message: String, data: Option[JsValue])

  def apply(code: Int,
            message: String,
            data: Option[JsValue],
            id: CorrelationId): JsonRpcResponseErrorMessage =
    apply(JsonRpcMessages.Version, Error(code, message, data), id)
}

final case class JsonRpcResponseMessageBatch(
  messages: Seq[JsonRpcResponseMessage]
) extends JsonRpcMessage {
  require(messages.nonEmpty)
}

object JsonRpcResponseErrorMessages {

  final val ReservedErrorCodeFloor: Int   = -32768
  final val ReservedErrorCodeCeiling: Int = -32000

  final val ParseErrorCode: Int         = -32700
  final val InvalidRequestCode: Int     = -32600
  final val MethodNotFoundCode: Int     = -32601
  final val InvalidParamsCode: Int      = -32602
  final val InternalErrorCode: Int      = -32603
  final val ServerErrorCodeFloor: Int   = -32099
  final val ServerErrorCodeCeiling: Int = -32000

  def parseError(exception: Throwable,
                 id: CorrelationId): JsonRpcResponseErrorMessage = rpcError(
    ParseErrorCode,
    message = "Parse error",
    meaning =
      "Invalid JSON was received by the server.\nAn error occurred on the server while parsing the JSON text.",
    error = Some(JsString(exception.getMessage)),
    id
  )

  def invalidRequest(exception: Throwable,
                     id: CorrelationId): JsonRpcResponseErrorMessage =
    rpcError(
      InvalidRequestCode,
      message = "Invalid Request",
      meaning = "The JSON sent is not a valid Request object.",
      error = Some(JsString(exception.getMessage)),
      id
    )

  def methodNotFound(method: String,
                     id: CorrelationId): JsonRpcResponseErrorMessage = rpcError(
    MethodNotFoundCode,
    message = "Method not found",
    meaning = "The method does not exist / is not available.",
    error = Some(JsString(s"""The method "$method" is not implemented.""")),
    id
  )

  def invalidParams(e: String, id: CorrelationId): JsonRpcResponseErrorMessage =
    rpcError(
      InvalidParamsCode,
      message = "Invalid params",
      meaning = "Invalid method parameter(s).",
      error = Some(JsString(e)),
      id
    )

  def internalError(error: Option[JsValue],
                    id: CorrelationId): JsonRpcResponseErrorMessage = rpcError(
    InternalErrorCode,
    message = "Internal error",
    meaning = "Internal JSON-RPC error.",
    error,
    id
  )

  def serverError(code: Int,
                  error: Option[JsValue],
                  id: CorrelationId): JsonRpcResponseErrorMessage = {
    require(code >= ServerErrorCodeFloor && code <= ServerErrorCodeCeiling)
    rpcError(
      code,
      message = "Server error",
      meaning = "Something went wrong in the receiving application.",
      error,
      id
    )
  }

  private def rpcError(code: Int,
                       message: String,
                       meaning: String,
                       error: Option[JsValue],
                       id: CorrelationId): JsonRpcResponseErrorMessage =
    JsonRpcResponseErrorMessage(
      code,
      message,
      data = Some(
        JsObject(
          ("meaning" -> JsString(meaning)) +:
            error.toSeq.map(error => "error" -> error): _*
        )
      ),
      id
    )

  def applicationError(code: Int,
                       message: String,
                       data: Option[JsValue],
                       id: CorrelationId): JsonRpcResponseErrorMessage = {
    require(code > ReservedErrorCodeCeiling || code < ReservedErrorCodeFloor)
    JsonRpcResponseErrorMessage(
      code,
      message,
      data,
      id
    )
  }
}
