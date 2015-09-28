package org.ensime.jerk

import spray.json._
import fommil.sjs._
import shapeless._

import org.ensime.api._

import org.ensime.util.file._

private object JerkConversions extends DefaultJsonProtocol with FamilyFormats {
  // wtf?? why is this needed, why does it even work? Miles??
  implicit val symbolFormat = SymbolJsonFormat

  // move to somewhere more general
  implicit object FileFormat extends JsonFormat[File] {
    def read(j: JsValue): File = j match {
      case JsString(path) => File(path)
      case other => unexpectedJson[File](other)
    }
    def write(f: File): JsValue = JsString(f.getPath)
  }

  // keeps the JSON a little bit cleaner
  implicit object DebugThreadIdFormat extends JsonFormat[DebugThreadId] {
    def read(j: JsValue): DebugThreadId = j match {
      case JsNumber(id) => new DebugThreadId(id.longValue)
      case other => unexpectedJson[DebugThreadId](other)
    }
    def write(dtid: DebugThreadId): JsValue = JsNumber(dtid.id)
  }

  // some of the case classes use the keyword `type`, so we need a better default
  override implicit def coproductHint[T: Typeable]: CoproductHint[T] = new FlatCoproductHint[T]("typehint")

  // note that cachedImplicit increased the compile time
  val RpcRequestFormat = RootJsonFormat[RpcRequest]
  val EnsimeServerMessageFormat = RootJsonFormat[EnsimeServerMessage]

}

object JerkFormats {
  implicit val RpcRequestFormat: RootJsonFormat[RpcRequest] =
    JerkConversions.RpcRequestFormat
  implicit val EnsimeServerMessageFormat: RootJsonFormat[EnsimeServerMessage] =
    JerkConversions.EnsimeServerMessageFormat
}

object JerkEnvelopeFormats {
  import JerkFormats._
  import DefaultJsonProtocol._

  // for the transitional SWANK (avoid derivations)
  implicit val RpcRequestEnvelopeFormat = jsonFormat2(RpcRequestEnvelope)
  implicit val RpcResponseEnvelopeFormat = jsonFormat2(RpcResponseEnvelope)

}
