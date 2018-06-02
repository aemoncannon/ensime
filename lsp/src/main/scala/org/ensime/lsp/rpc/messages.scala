// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.rpc.messages

import scala.util._

import spray.json._
import JsWriter.ops._
import JsReader.ops._
import DeserializationException._

import scala.collection.immutable.Seq

import scalaz.deriving

object JsonRpcMessages {
  sealed trait Version
  object Version {
    case object `2.0` extends Version
    implicit val jsReader: JsReader[Version] =
      JsReader[String]
        .filter(_ == "2.0", "Version should be 2.0")
        .map(_ => `2.0`)
    implicit val jsWriter: JsWriter[Version] = {
      case `2.0` => JsString("2.0")
    }
  }
}

sealed trait CorrelationId
case object NullId                            extends CorrelationId
final case class NumberId(number: BigDecimal) extends CorrelationId
final case class StringId(str: String)        extends CorrelationId

object CorrelationId {
  def apply(): CorrelationId                   = NullId
  def apply(number: BigDecimal): CorrelationId = NumberId(number)
  def apply(str: String): CorrelationId        = StringId(str)

  implicit val jsWriter: JsWriter[CorrelationId] = {
    case NullId        => JsNull
    case NumberId(num) => JsNumber(num)
    case StringId(str) => JsString(str)
  }
  implicit val jsReader: JsReader[CorrelationId] = {
    case JsNull        => Right(NullId)
    case JsNumber(num) => Right(NumberId(num))
    case JsString(str) => Right(StringId(str))
    case _             => Left(deserError[CorrelationId]("Wrong CorrelationId format"))
  }

}

sealed trait Params
final case class ObjectParams(obj: JsObject) extends Params
final case class ArrayParams(arr: JsArray)   extends Params

object Params {
  def apply(): Option[Params]              = None
  def apply(obj: JsObject): Option[Params] = Some(ObjectParams(obj))
  def apply(arr: JsArray): Option[Params]  = Some(ArrayParams(arr))

  implicit val jsWriter: JsWriter[Params] = {
    case ObjectParams(obj) => obj
    case ArrayParams(arr)  => arr
  }
  implicit val jsReader: JsReader[Params] = {
    case obj @ JsObject(_) => Right(ObjectParams(obj))
    case arr @ JsArray(_)  => Right(ArrayParams(arr))
    case _                 => Left(deserError[Params]("Wrong Params format"))
  }

}

sealed abstract class JsonRpcMessage
object JsonRpcMessage {
  implicit val jsWriter: JsWriter[JsonRpcMessage] = {
    case req: JsonRpcRequestMessage         => req.toJson
    case n: JsonRpcNotificationMessage      => n.toJson
    case suc: JsonRpcResponseSuccessMessage => suc.toJson
    case e: JsonRpcResponseErrorMessage     => e.toJson
    case breq: JsonRpcRequestMessageBatch   => breq.toJson
    case bres: JsonRpcResponseMessageBatch  => bres.toJson
  }

  implicit val jsReader: JsReader[JsonRpcMessage] = { j =>
    {
      j.as[JsonRpcRequestMessage].toOption orElse
        j.as[JsonRpcNotificationMessage].toOption orElse
        j.as[JsonRpcResponseSuccessMessage].toOption orElse
        j.as[JsonRpcResponseErrorMessage].toOption orElse
        j.as[JsonRpcRequestMessageBatch].toOption orElse
        j.as[JsonRpcResponseMessageBatch].toOption
    } match {
      case None =>
        Left(deserError[JsonRpcMessage]("Error during JsonRpcMessage parsing"))
      case Some(x) => Right(x)
    }
  }
}

sealed trait JsonRpcRequestOrNotificationMessage
object JsonRpcRequestOrNotificationMessage {
  implicit val jsWriter: JsWriter[JsonRpcRequestOrNotificationMessage] = {
    case r: JsonRpcRequestMessage      => r.toJson
    case n: JsonRpcNotificationMessage => n.toJson
  }
  implicit val jsReader: JsReader[JsonRpcRequestOrNotificationMessage] = {
    case j @ JsObject(fields) =>
      if (fields.contains("id"))
        j.as[JsonRpcRequestMessage]
      else
        j.as[JsonRpcNotificationMessage]
    case _ =>
      Left(
        deserError[JsonRpcRequestOrNotificationMessage](
          "Response message should be an object"
        )
      )
  }
}

@deriving(JsReader, JsWriter)
final case class JsonRpcRequestMessage(jsonrpc: JsonRpcMessages.Version,
                                       method: String,
                                       params: Option[Params],
                                       id: CorrelationId)
    extends JsonRpcMessage
    with JsonRpcRequestOrNotificationMessage {
  require(id != NullId, "Request message id cannot be null")
}
object JsonRpcRequestMessage {
  def apply(method: String,
            params: Option[Params],
            id: CorrelationId): JsonRpcRequestMessage =
    apply(JsonRpcMessages.Version.`2.0`, method, params, id)
}

@deriving(JsReader, JsWriter)
final case class JsonRpcNotificationMessage(
  jsonrpc: JsonRpcMessages.Version,
  method: String,
  params: Option[Params]
) extends JsonRpcMessage
    with JsonRpcRequestOrNotificationMessage
object JsonRpcNotificationMessage {
  def apply(method: String,
            params: Option[Params]): JsonRpcNotificationMessage =
    apply(JsonRpcMessages.Version.`2.0`, method, params)
}

final case class JsonRpcRequestMessageBatch(
  messages: Seq[JsonRpcRequestOrNotificationMessage]
) extends JsonRpcMessage {
  require(messages.nonEmpty, "Request batch messages cannot be empty")
}
object JsonRpcRequestMessageBatch {
  implicit val jsWriter: JsWriter[JsonRpcRequestMessageBatch] =
    JsWriter[Seq[JsonRpcRequestOrNotificationMessage]].contramap(_.messages)
  implicit val jsReader: JsReader[JsonRpcRequestMessageBatch] =
    JsReader[Seq[JsonRpcRequestOrNotificationMessage]]
      .filter(_.nonEmpty, "Request batch should not be empty")
      .map(JsonRpcRequestMessageBatch(_))
}

sealed abstract class JsonRpcResponseMessage extends JsonRpcMessage {
  def id: CorrelationId
}
object JsonRpcResponseMessage {

  implicit val jsWriter: JsWriter[JsonRpcResponseMessage] = {
    case r: JsonRpcResponseSuccessMessage => r.toJson
    case e: JsonRpcResponseErrorMessage   => e.toJson
  }
  implicit val jsReader: JsReader[JsonRpcResponseMessage] = {
    case j @ JsObject(fields) =>
      if (fields.contains("error"))
        j.as[JsonRpcResponseErrorMessage]
      else
        j.as[JsonRpcResponseSuccessMessage]
    case _ =>
      Left(
        deserError[JsonRpcResponseMessage](
          "Response message should be an object"
        )
      )
  }

}

@deriving(JsReader, JsWriter)
final case class JsonRpcResponseSuccessMessage(jsonrpc: JsonRpcMessages.Version,
                                               result: JsValue,
                                               id: CorrelationId)
    extends JsonRpcResponseMessage
object JsonRpcResponseSuccessMessage {
  def apply(result: JsValue, id: CorrelationId): JsonRpcResponseSuccessMessage =
    apply(JsonRpcMessages.Version.`2.0`, result, id)
}

@deriving(JsReader, JsWriter)
final case class JsonRpcResponseErrorMessage(
  jsonrpc: JsonRpcMessages.Version,
  error: JsonRpcResponseErrorMessage.Error,
  id: CorrelationId
) extends JsonRpcResponseMessage
object JsonRpcResponseErrorMessage {
  @deriving(JsReader, JsWriter)
  final case class Error(code: Int, message: String, data: Option[JsValue])

  def apply(code: Int,
            message: String,
            data: Option[JsValue],
            id: CorrelationId): JsonRpcResponseErrorMessage =
    apply(JsonRpcMessages.Version.`2.0`, Error(code, message, data), id)
}

final case class JsonRpcResponseMessageBatch(
  messages: Seq[JsonRpcResponseMessage]
) extends JsonRpcMessage {
  require(messages.nonEmpty, "Request batch messages cannot be empty")
}
object JsonRpcResponseMessageBatch {
  implicit val jsWriter: JsWriter[JsonRpcResponseMessageBatch] =
    JsWriter[Seq[JsonRpcResponseMessage]].contramap(_.messages)
  implicit val jsReader: JsReader[JsonRpcResponseMessageBatch] =
    JsReader[Seq[JsonRpcResponseMessage]]
      .filter(_.nonEmpty, "Request batch messages cannot be empty")
      .map(JsonRpcResponseMessageBatch(_))
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

  def invalidRequest(exception: DeserializationException,
                     id: CorrelationId): JsonRpcResponseErrorMessage =
    rpcError(
      InvalidRequestCode,
      message = "Invalid Request",
      meaning = "The JSON sent is not a valid Request object.",
      error = Some(JsString(exception.msg)),
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
