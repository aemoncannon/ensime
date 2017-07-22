// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import scala.collection.JavaConverters._

import com.sun.source.tree._
import com.sun.source.tree.Tree.Kind
import com.sun.source.util.TreePath

object ScopeFor {

  def apply(compilation: Compilation, position: Int): Option[Scope] = {

    PathFor(compilation, position) map { path =>

      val sourcePositions = compilation.trees.getSourcePositions

      val root = path.getCompilationUnit

      val statements: List[_ <: StatementTree] = path.getLeaf.getKind match {
        case Kind.BLOCK => path.getLeaf.asInstanceOf[BlockTree].getStatements.asScala.toList
        case Kind.FOR_LOOP => path.getLeaf.asInstanceOf[ForLoopTree].getInitializer.asScala.toList
        case Kind.ENHANCED_FOR_LOOP => List(path.getLeaf.asInstanceOf[EnhancedForLoopTree].getStatement)
        case Kind.CASE => path.getLeaf.asInstanceOf[CaseTree].getStatements.asScala.toList
        case Kind.METHOD => path.getLeaf.asInstanceOf[MethodTree].getParameters.asScala.toList
        case otherwise => Nil
      }

      val isAtPosition: StatementTree => Boolean = {
        sourcePositions.getStartPosition(root, _) <= position
      }

      val treePathOption = for {
        tree <- statements.reverse.find(isAtPosition)
      } yield new TreePath(path, tree)

      // FIXME: It's just a workaround to let Emacs not disconnect to ensime-server, but the result is not correct. #1764
      treePathOption match {
        case Some(newPath) => compilation.trees.getScope(treePathOption.getOrElse(path))
        case _ => throw new IllegalStateException("Unexpected response type from request.")
      }
    }
  }
}
