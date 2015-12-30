package org.ensime.core

import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.reflect.io.AbstractFile
import scala.tools.refactoring.common.{ CompilerAccess, PimpedTrees }
import scala.reflect.internal.util.SourceFile
import org.ensime.api._

class StructureViewBuilder(val global: RichPresentationCompiler)
    extends CompilerAccess
    with PimpedTrees {

  import global._

  sealed trait MemberBuilder {
    def build: StructureViewMember
  }

  case class DefsBuilder(
      keyword: String,
      name: String,
      sym: Symbol,
      members: ListBuffer[MemberBuilder]
  ) extends MemberBuilder {
    def build: StructureViewMember =
      StructureViewMember(
        keyword,
        name,
        locateSymbolPos(sym, PosNeededYes).getOrElse(EmptySourcePosition()),
        members.map(_.build).toList
      )
  }

  class StructureTraverser() extends Traverser {
    val log = LoggerFactory.getLogger(getClass)

    val stucture = new ListBuffer[StructureViewMember]()

    override def traverse(tree: Tree): Unit = {
      val df = DefsBuilder("", "", tree.symbol, new ListBuffer())
      traverse(tree, df)
      stucture.appendAll(df.members.map(_.build))
    }

    def shouldShow(x: DefDef): Boolean =
      !(x.name == nme.CONSTRUCTOR || x.name == nme.MIXIN_CONSTRUCTOR || x.symbol.isAccessor)

    private def traverse(tree: Tree, parent: DefsBuilder): Unit = {
      tree match {
        case x: DefTree if x.symbol.isSynthetic =>
        case x: ImplDef =>
          val df = DefsBuilder(x.keyword, x.name.toString, x.symbol, new ListBuffer())
          parent.members.append(df)
          x.impl.body.foreach(traverse(_, df))
        case x: DefDef if shouldShow(x) =>
          parent.members.append(DefsBuilder(x.keyword, x.name.toString, x.symbol, new ListBuffer()))
        case x: TypeDef =>
          parent.members.append(DefsBuilder(x.keyword, x.name.toString, x.symbol, new ListBuffer()))
        case _ =>
          tree.children.foreach(traverse(_, parent))
      }
    }
  }

  def build(fileInfo: SourceFile): List[StructureViewMember] = {
    def getStructureTree(f: SourceFile) = {
      val x = new Response[Tree]()
      askStructure(true)(f, x)
      x.get
    }

    getStructureTree(fileInfo) match {
      case Left(tree) =>
        val traverser = new StructureTraverser()
        traverser.traverse(tree)
        traverser.stucture.toList
      case Right(ex) => List.empty
    }
  }

  def compilationUnitOfFile(f: AbstractFile): Option[CompilationUnit] = unitOfFile.get(f)
}
