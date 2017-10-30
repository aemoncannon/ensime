// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.rpc

import org.ensime.lsp.JsonUtils
import org.ensime.lsp.rpc.messages._
import shapeless._
import spray.json._

import scala.util.{ Failure, Success, Try }

private object RpcConversions
    extends DefaultJsonProtocol
    with FamilyFormats
    with AdditionalFormats {

  // see JerkyFormats for an explanation of this "accidental complexity"
  implicit override def eitherFormat[A: JsonFormat, B: JsonFormat]
    : JsonFormat[Either[A, B]]                                = super.eitherFormat[A, B]
  implicit val highPriorityJsValue: JsonFormat[JsValue]       = jsValue
  implicit val highPriorityJsObject: RootJsonFormat[JsObject] = jsObject
  implicit val highPriorityJsArray: RootJsonFormat[JsArray]   = jsArray

  implicit val CorrelationIdFormat: JsonFormat[CorrelationId] =
    JsonFormat.instance[CorrelationId] {
      case NullId        => JsNull
      case NumberId(num) => JsNumber(num)
      case StringId(str) => JsString(str)
    } {
      case JsNull        => NullId
      case JsNumber(num) => NumberId(num)
      case JsString(str) => StringId(str)
      case _             => deserError[CorrelationId]("Wrong CorrelationId format")
    }

  implicit val ParamsFormat: JsonFormat[Params] =
    JsonFormat.instance[Params] {
      case NullParams        => JsNull
      case ObjectParams(obj) => obj
      case ArrayParams(arr)  => arr
    } {
      case JsNull                             => NullParams
      case JsObject(fields) if fields.isEmpty => NullParams
      case obj @ JsObject(_)                  => ObjectParams(obj)
      case arr @ JsArray(_)                   => ArrayParams(arr)
      case _                                  => deserError[Params]("Wrong Params format")
    }

  implicit val JsonRpcResponseMessageFormat
    : RootJsonFormat[JsonRpcResponseMessage] =
    RootJsonFormat.instance[JsonRpcResponseMessage] {
      case r: JsonRpcResponseSuccessMessage => r.toJson
      case e: JsonRpcResponseErrorMessage   => e.toJson
    } {
      case j @ JsObject(fields) =>
        if (fields.contains("error")) {
          j.convertTo[JsonRpcResponseErrorMessage]
        } else {
          j.convertTo[JsonRpcResponseSuccessMessage]
        }
      case _ =>
        deserError[JsonRpcResponseMessage](
          "Response message should be an object"
        )
    }

  implicit val JsonRpcRequestOrNotificationMessageFormat
    : RootJsonFormat[JsonRpcRequestOrNotificationMessage] =
    RootJsonFormat.instance[JsonRpcRequestOrNotificationMessage] {
      case r: JsonRpcRequestMessage      => r.toJson
      case n: JsonRpcNotificationMessage => n.toJson
    } {
      case j @ JsObject(fields) =>
        if (fields.contains("id")) {
          j.convertTo[JsonRpcRequestMessage]
        } else {
          j.convertTo[JsonRpcNotificationMessage]
        }
      case _ =>
        deserError[JsonRpcRequestOrNotificationMessage](
          "Response message should be an object"
        )
    }

  implicit val JsonRpcRequestMessageBatchFormat
    : RootJsonFormat[JsonRpcRequestMessageBatch] =
    rootFormat(JsonUtils.wrapperFormat(JsonRpcRequestMessageBatch, _.messages))

  implicit val JsonRpcResponseMessageBatchFormat
    : RootJsonFormat[JsonRpcResponseMessageBatch] =
    rootFormat(JsonUtils.wrapperFormat(JsonRpcResponseMessageBatch, _.messages))

  implicit val JsonRpcMessageFormat: RootJsonFormat[JsonRpcMessage] =
    RootJsonFormat.instance[JsonRpcMessage] {
      case req: JsonRpcRequestMessage         => req.toJson
      case n: JsonRpcNotificationMessage      => n.toJson
      case suc: JsonRpcResponseSuccessMessage => suc.toJson
      case e: JsonRpcResponseErrorMessage     => e.toJson
      case breq: JsonRpcRequestMessageBatch   => breq.toJson
      case bres: JsonRpcResponseMessageBatch  => bres.toJson
    } { j =>
      // we should not introduce new fields to rpc jsons
      val tryAll = Try(j.convertTo[JsonRpcRequestMessage]) orElse
        Try(j.convertTo[JsonRpcNotificationMessage]) orElse
        Try(j.convertTo[JsonRpcResponseSuccessMessage]) orElse
        Try(j.convertTo[JsonRpcResponseErrorMessage]) orElse
        Try(j.convertTo[JsonRpcRequestMessageBatch]) orElse
        Try(j.convertTo[JsonRpcResponseMessageBatch])

      tryAll match {
        case Failure(_) =>
          deserError[JsonRpcMessage]("Error during JsonRpcMessage parsing")
        case Success(x) => x
      }
    }

  implicit val JsonRpcRequestMessageFormat
    : RootJsonFormat[JsonRpcRequestMessage] = cachedImplicit
  implicit val JsonRpcNotificationMessageFormat
    : RootJsonFormat[JsonRpcNotificationMessage] = cachedImplicit
  implicit val JsonRpcResponseSuccessMessageFormat
    : RootJsonFormat[JsonRpcResponseSuccessMessage] = cachedImplicit
  implicit val JsonRpcResponseErrorMessageFormat
    : RootJsonFormat[JsonRpcResponseErrorMessage] = cachedImplicit

}

object RpcFormats {
  implicit val JsonRpcMessageFormat: RootJsonFormat[JsonRpcMessage] =
    RpcConversions.JsonRpcMessageFormat
  implicit val JsonRpcRequestMessageFormat
    : RootJsonFormat[JsonRpcRequestMessage] =
    RpcConversions.JsonRpcRequestMessageFormat
  implicit val JsonRpcNotificationMessageFormat
    : RootJsonFormat[JsonRpcNotificationMessage] =
    RpcConversions.JsonRpcNotificationMessageFormat
  implicit val JsonRpcResponseMessage: RootJsonFormat[JsonRpcResponseMessage] =
    RpcConversions.JsonRpcResponseMessageFormat
  implicit val JsonRpcResponseSuccessMessageFormat
    : RootJsonFormat[JsonRpcResponseSuccessMessage] =
    RpcConversions.JsonRpcResponseSuccessMessageFormat
  implicit val JsonRpcResponseErrorMessageFormat
    : RootJsonFormat[JsonRpcResponseErrorMessage] =
    RpcConversions.JsonRpcResponseErrorMessageFormat

  implicit val JsonRpcRequestMessageBatchFormat
    : RootJsonFormat[JsonRpcRequestMessageBatch] =
    RpcConversions.JsonRpcRequestMessageBatchFormat
  implicit val JsonRpcResponseMessageBatchFormat
    : RootJsonFormat[JsonRpcResponseMessageBatch] =
    RpcConversions.JsonRpcResponseMessageBatchFormat
}
