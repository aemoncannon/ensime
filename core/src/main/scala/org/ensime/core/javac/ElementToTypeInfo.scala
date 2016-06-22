package org.ensime.core.javac

import org.ensime.api.TypeInfo
import org.ensime.api.ArrowTypeInfo
import org.ensime.api.ParamSectionInfo

import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import javax.lang.model.`type`.TypeMirror

import scala.collection.JavaConversions._

trait ElementToTypeInfo[E <: Element] {

  def apply(e: E)(implicit
    methodNameFormat: ElementNameFormat[ExecutableElement],
    typeNameFormat: TypeNameFormat[TypeMirror]): TypeInfo

}

object DefaultElementToTypeInfo extends ElementToTypeInfo[Element] {

  def apply(e: Element)(implicit
    methodNameFormat: ElementNameFormat[ExecutableElement],
    typeNameFormat: TypeNameFormat[TypeMirror]) = DefaultTypeMirrorToTypeInfo(e.asType())

}

object MethodToTypeInfo extends ElementToTypeInfo[ExecutableElement] {

  def apply(e: ExecutableElement)(implicit
    methodNameFormat: ElementNameFormat[ExecutableElement],
    typeNameFormat: TypeNameFormat[TypeMirror]): TypeInfo = {

    val javaMethodName = methodNameFormat(e)

    ArrowTypeInfo(
      javaMethodName.short, javaMethodName.full,
      DefaultTypeMirrorToTypeInfo(e.getReturnType),
      ParamSectionInfo(
        e.getParameters.map { param =>
          param.getSimpleName.toString ->
            DefaultTypeMirrorToTypeInfo(param.asType)
        },
        isImplicit = false
      ) :: Nil
    )
  }
}

object ElementToTypeInfo {

  def apply(e: Element)(
    implicit
    methodNameFormat: ElementNameFormat[ExecutableElement],
    typeNameFormat: TypeNameFormat[TypeMirror]
  ): TypeInfo = e match {

    case e: ExecutableElement => MethodToTypeInfo(e)
    case e: Element => DefaultElementToTypeInfo(e)
  }
}

