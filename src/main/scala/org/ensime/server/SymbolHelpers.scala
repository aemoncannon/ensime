package org.ensime.model

object SymbolHelpers {
  import scala.tools.nsc.symtab.Flags._

  val Method = 'method
  val Trait = 'trait
  val Interface = 'interface
  val Object = 'object
  val Class = 'class
  val Field = 'field
  val Nil = 'nil
}
