// Copyright (C) 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

/**
 * Type safety around Scala type names, scalac uses raw Strings.
 */
final class ScalaName(val underlying: String) extends AnyVal {
  def +(other: String): ScalaName =
    if (other == "") this
    else new ScalaName(underlying + other)
}

trait TypeToScalaName { self: Global with Helpers =>
  def scalaName(tpe: Type, full: Boolean): ScalaName = {
    if (isArrowType(tpe)) {
      val tparams = tpe.paramss.map { sect =>
        sect.map { p => scalaName(p.tpe, full).underlying } match {
          case one :: Nil => one
          case many => many.mkString("(", ", ", ")")
        }
      }.mkString(" => ")
      new ScalaName(tparams) + " => " + scalaName(tpe.finalResultType, full).underlying
    } else {
      val name = if (full) tpe.typeSymbol.fullName else tpe.typeSymbol.nameString
      new ScalaName(name) + {
        if (tpe.typeArgs.nonEmpty)
          tpe.typeArgs.map(scalaName(_, full).underlying).mkString("[", ", ", "]")
        else ""
      }
    }
  }

  def fullName(tpe: Type): ScalaName = scalaName(tpe, full = true)
  def shortName(tpe: Type): ScalaName = scalaName(tpe, full = false)

  def shortName(sym: Symbol): ScalaName = new ScalaName(sym.nameString)

}
