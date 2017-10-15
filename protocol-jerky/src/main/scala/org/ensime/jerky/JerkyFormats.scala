// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.jerky

import spray.json._
import shapeless._
import org.ensime.api._
import org.ensime.util.file._
import org.ensime.util.ensimefile._

private object JerkyConversions extends DefaultJsonProtocol with FamilyFormats {
  // This part of the code is brought to you by the words "accidental"
  // and "complexity".
  //
  // Lack of definition in scalac's implicit resolution rules means
  // that we have to redefine some things here.
  implicit override def eitherFormat[A: JsonFormat, B: JsonFormat]
    : JsonFormat[Either[A, B]]                              = super.eitherFormat[A, B]
  implicit val highPrioritySymbolFormat: JsonFormat[Symbol] = symbol

  implicit val FileFormat: JsonFormat[File] =
    JsonFormat.instance[File](f => JsString(f.getPath)) {
      case JsString(path) => File(path)
      case other          => unexpectedJson[File](other)
    }
  // clients appreciate a simpler format for files
  implicit val EnsimeFileFormat: JsonFormat[EnsimeFile] =
    JsonFormat.instance[EnsimeFile] {
      case RawFile(path)  => JsString(path.toString)
      case a: ArchiveFile => JsString(a.uriString)
    } {
      case JsString(uri) => EnsimeFile(uri)
      case got           => unexpectedJson[EnsimeFile](got)
    }
  // keeps the JSON a little bit cleaner
  implicit val DebugThreadIdFormat: JsonFormat[DebugThreadId] =
    JsonFormat.instance[DebugThreadId](d => JsNumber(d.id)) {
      case JsNumber(id) => new DebugThreadId(id.longValue)
      case other        => unexpectedJson[DebugThreadId](other)
    }

  // some of the case classes use the keyword `type`, so we need a better default
  override implicit def coproductHint[T: Typeable]: CoproductHint[T] =
    new FlatCoproductHint[T]("typehint")

  implicit val RpcRequestEnvelopeFormat: RootJsonFormat[RpcRequestEnvelope] =
    cachedImplicit
  implicit val RpcResponseEnvelopeFormat: RootJsonFormat[RpcResponseEnvelope] =
    cachedImplicit

}

object JerkyFormats {
  implicit val RpcRequestEnvelopeFormat: RootJsonFormat[RpcRequestEnvelope] =
    JerkyConversions.RpcRequestEnvelopeFormat
  implicit val RpcResponseEnvelopeFormat: RootJsonFormat[RpcResponseEnvelope] =
    JerkyConversions.RpcResponseEnvelopeFormat
}
