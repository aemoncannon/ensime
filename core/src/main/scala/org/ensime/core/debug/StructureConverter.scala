package org.ensime.core.debug

import org.ensime.api._
import org.scaladebugger.api.dsl.Implicits._
import org.scaladebugger.api.profiles.traits.info._

/**
 * Converts normal debugger API structures into their equivalent Ensime-oriented
 * messages.
 *
 * @param sourceMap Used to include relevant source information when
 *                  constructing various Ensime messages
 */
class StructureConverter(private val sourceMap: SourceMap) {
  /**
   * Converts a debugger API value into an Ensime message.
   *
   * @param valueInfo The debugger API value
   * @return The equivalent Ensime message
   */
  def convertValue(valueInfo: ValueInfoProfile): DebugValue = {
    valueInfo match {
      case v if v.isNull => newDebugNull()
      case v if v.isVoid => convertDebugVoid(v)
      case v if v.isArray => convertDebugArr(v.toArrayInfo)
      case v if v.isString => convertDebugStr(v.toStringInfo)
      case v if v.isObject => convertDebugObj(v.toObjectInfo)
      case v if v.isPrimitive => convertDebugPrim(v.toPrimitiveInfo)
    }
  }

  /**
   * Converts a remote object value to an equivalent Ensime message.
   *
   * @param value The object value to convert
   * @return The new Ensime message
   */
  private def convertDebugObj(value: ObjectInfoProfile): DebugObjectInstance = {
    DebugObjectInstance(
      value.toPrettyString,
      extractFields(value.referenceType, value),
      value.referenceType.name,
      DebugObjectId(value.uniqueId)
    )
  }

  /**
   * Converts a remote string value to an equivalent Ensime message.
   *
   * @param value The string value to convert
   * @return The new Ensime message
   */
  private def convertDebugStr(value: StringInfoProfile): DebugStringInstance = {
    DebugStringInstance(
      value.toPrettyString,
      extractFields(value.referenceType, value),
      value.referenceType.name,
      DebugObjectId(value.uniqueId)
    )
  }

  /**
   * Converts a remote array to an equivalent Ensime message.
   *
   * @param value The array to convert
   * @return The new Ensime message
   */
  private def convertDebugArr(value: ArrayInfoProfile): DebugArrayInstance = {
    DebugArrayInstance(
      value.length,
      value.referenceType.name,
      value.referenceType.toArrayType.elementTypeName,
      DebugObjectId(value.uniqueId)
    )
  }

  /**
   * Converts a remote primitive value to an equivalent Ensime message.
   *
   * @param value The primitive value to convert
   * @return The new Ensime message
   */
  private def convertDebugPrim(value: PrimitiveInfoProfile): DebugPrimitiveValue = {
    DebugPrimitiveValue(
      value.toPrettyString,
      value.typeInfo.name
    )
  }

  /**
   * Converts a remote void value to an equivalent Ensime message.
   *
   * @param value The void value to convert
   * @return The new Ensime message
   */
  private def convertDebugVoid(value: ValueInfoProfile): DebugPrimitiveValue = {
    DebugPrimitiveValue(
      value.toPrettyString,
      value.typeInfo.name
    )
  }

  /**
   * Creates a new Ensime message representing a null value.
   *
   * @return The new Ensime message
   */
  private def newDebugNull(): DebugNullValue = {
    DebugNullValue("Null")
  }

  /**
   * Extracts fields and their values from an object and its reference type.
   *
   * @param tpeIn The reference type for the object whose fields to acquire
   * @param obj The actual remote object whose values for corresponding fields
   *            to convert to messages
   * @return The collection of Ensime messages representing the fields
   */
  private def extractFields(
    tpeIn: ReferenceTypeInfoProfile,
    obj: ObjectInfoProfile
  ): List[DebugClassField] = {
    if (!tpeIn.isClassType) return List.empty

    var fields = List[DebugClassField]()
    var tpe: Option[ClassTypeInfoProfile] = Some(tpeIn.toClassType)
    while (tpe.nonEmpty) {
      fields = tpe.map(_.indexedVisibleFields)
        .map(s => s.map(f => DebugClassField(
          f.offsetIndex,
          f.name,
          f.typeName,

          // NOTE: Try to get static fields (from reference type) and instance
          //       fields (from object instance)
          f.tryToValueInfo.orElse(
            obj.tryField(f.name).flatMap(_.tryToValueInfo)
          ).map(_.toPrettyString).getOrElse("???")
        ))).getOrElse(Nil).toList ++ fields

      tpe = tpe.flatMap(_.superclassOption)
    }
    fields
  }

  /**
   * Converts a single stack frame to an Ensime message.
   *
   * @param frame The stack frame to convert
   * @return The resulting Ensime message
   */
  def convertStackFrame(frame: FrameInfoProfile): DebugStackFrame = {
    val locals = ignoreErr(
      frame.indexedLocalVariables.map(convertStackLocal).toList,
      List.empty
    )

    val numArgs = ignoreErr(frame.argumentValues.length, 0)
    val methodName = ignoreErr(frame.location.method.name, "Method")
    val className = ignoreErr(frame.location.declaringType.name, "Class")

    import org.ensime.util.file._
    val pcLocation = sourceMap.newLineSourcePosition(frame.location).getOrElse(
      LineSourcePosition(
        File(frame.location.sourcePath).canon,
        frame.location.lineNumber
      )
    )
    val thisObjId = ignoreErr(frame.thisObject.cache().uniqueId, -1L)
    DebugStackFrame(frame.index, locals, numArgs, className, methodName, pcLocation, DebugObjectId(thisObjId))
  }

  /**
   * Converts a local variable on the stack to an Ensime message.
   *
   * @param variableInfo The local variable to convert
   * @return The resulting Ensime message
   */
  private def convertStackLocal(
    variableInfo: IndexedVariableInfoProfile
  ): DebugStackLocal = {
    DebugStackLocal(
      variableInfo.offsetIndex,
      variableInfo.name,
      variableInfo.toValueInfo.toPrettyString,
      variableInfo.typeName
    )
  }

  /**
   * Executes the provided action, yielding a executing a different action if
   * it fails.
   *
   * @param action The action to execute
   * @param orElse The other action to execute if the first fails
   * @tparam T The return type of both actions
   * @return The result from executing the first or second action
   */
  private def ignoreErr[T](action: => T, orElse: => T): T = {
    try { action } catch { case e: Exception => orElse }
  }
}
