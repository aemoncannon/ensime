// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import javax.lang.model.`type`.TypeMirror

import org.ensime.api.TypeInfo
import org.ensime.api.DeclaredAs
import org.ensime.api.BasicTypeInfo

trait TypeMirrorToTypeInfo[T <: TypeMirror] {

  def apply(t: T)(implicit nameFormat: TypeNameFormat[T]): TypeInfo

}

object DefaultTypeMirrorToTypeInfo extends TypeMirrorToTypeInfo[TypeMirror] {

  def apply(t: TypeMirror)(implicit nameFormat: TypeNameFormat[TypeMirror]) = {
    val name = nameFormat(t)
    BasicTypeInfo(name.short, DeclaredAs.Class, name.full, Nil, Nil, None)
  }
}

object TypeMirrorToTypeInfo {

  implicit val defaultTypeMirrorToTypeInfo = DefaultTypeMirrorToTypeInfo

}
