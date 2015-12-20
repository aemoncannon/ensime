// Copyright (C) 2016 ENSIME Authors
// License: GPL 3.0
package org.ensime.core

import org.ensime.indexer._
import scala.tools.nsc.interactive.Global
import org.slf4j.Logger

/**
 * Resolves scalac [[scala.reflect.internal.Symbols.Symbol]] to Java
 * bytecode FQNs (including descriptors for methods).
 *
 * Note this does not work for
 * [[scala.tools.scalap.scalax.rules.scalasig.Symbol]] which must be
 * handled separately (going from a `Symbols.Symbol` to a
 * `scalasig.Symbol` would involve invoking the `Pickler` phase -
 * which we can't do - to generate a byte array that could be
 * reparsed by scalap).
 *
 * See also `DocFinding.javaFqnString` (which should be rewritten to
 * use this).
 */
trait SymbolToFqn { self: Global =>
  def logger: Logger

  import ClassName._
  private val ScalaPackageName = PackageName(List("scala"))
  private val normaliseClass: ClassName => ClassName = Map(
    ClassName(PackageName(List("scala", "runtime")), "BoxedUnit") -> PrimitiveVoid,
    ClassName(ScalaPackageName, "<byname>") -> ClassName(ScalaPackageName, "Function0"),
    ClassName(ScalaPackageName, "Boolean") -> PrimitiveBoolean,
    ClassName(ScalaPackageName, "Byte") -> PrimitiveByte,
    ClassName(ScalaPackageName, "Char") -> PrimitiveChar,
    ClassName(ScalaPackageName, "Short") -> PrimitiveShort,
    ClassName(ScalaPackageName, "Int") -> PrimitiveInt,
    ClassName(ScalaPackageName, "Long") -> PrimitiveLong,
    ClassName(ScalaPackageName, "Float") -> PrimitiveFloat,
    ClassName(ScalaPackageName, "Double") -> PrimitiveDouble,
    ClassName(ScalaPackageName, "Void") -> PrimitiveVoid
  ).withDefault(identity)

  private def className(sym: Symbol): ClassName = {
    // TODO: resolve arrays to their reified class
    val (pkg, name) = sym.javaClassName.split("[.]").toList.reverse match {
      case last :: init => (init.reverse, last)
      case _ => throw new IllegalArgumentException(sym.nameString)
    }
    val postfix = if (sym.isModuleOrModuleClass) "$" else ""
    normaliseClass(ClassName(PackageName(pkg), name + postfix))
  }

  private def descriptorType(t: Type): DescriptorType = {
    val c = className(t.dealias.erasure.typeSymbol)
    if (c.fqnString == "scala.Array") {
      ArrayDescriptor(descriptorType(t.typeArgs.head))
    } else c
  }

  private def methodName(sym: MethodSymbol): MethodName = {
    // handles special characters in method names, e.g. $plus
    // also ensures that scala primitive wrappers are not resolved when the base
    val owner = sym.ownerChain.dropWhile(_.isMethod).head
    val (pkg, clazz, name) = sym.javaClassName.split("[.]").toList.reverse match {
      case last :: init if init.nonEmpty => (init.tail.reverse, init.head, last)
      case _ => throw new IllegalArgumentException(sym.nameString)
    }
    val descriptor: Descriptor = {
      val params = sym.paramLists.flatten.map {
        p => descriptorType(p.tpe)
      }
      val ret = descriptorType(sym.returnType)
      Descriptor(params, ret)
    }
    val postfix = if (owner.isModuleOrModuleClass) "$" else ""
    MethodName(ClassName(PackageName(pkg), clazz + postfix), name, descriptor)
  }

  private def fieldName(sym: TermSymbol): FieldName = {
    ???
  }

  def toFqn(sym: Symbol): FullyQualifiedName = sym match {
    case ts: TypeSymbol => className(ts)
    case ms: ModuleSymbol => className(ms)
    case ms: MethodSymbol => methodName(ms)
    case ts: TermSymbol => fieldName(ts)
  }

}
