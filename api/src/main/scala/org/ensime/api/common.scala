// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File
import java.nio.file.Path

import scala.annotation.StaticAnnotation

import spray.json._

import scalaz.deriving

/**
 * Indicates that something will be removed.
 *
 * WORKAROUND https://issues.scala-lang.org/browse/SI-7934
 */
class deprecating(val detail: String = "") extends StaticAnnotation

@deriving(JsReader, JsWriter)
sealed abstract class DeclaredAs(val symbol: scala.Symbol)

object DeclaredAs {
  @deriving(JsReader, JsWriter)
  case object Method extends DeclaredAs('method)
  @deriving(JsReader, JsWriter)
  case object Trait extends DeclaredAs('trait)
  @deriving(JsReader, JsWriter)
  case object Interface extends DeclaredAs('interface)
  @deriving(JsReader, JsWriter)
  case object Object extends DeclaredAs('object)
  @deriving(JsReader, JsWriter)
  case object Class extends DeclaredAs('class)
  @deriving(JsReader, JsWriter)
  case object Field extends DeclaredAs('field)
  @deriving(JsReader, JsWriter)
  case object Nil extends DeclaredAs('nil)

  def allDeclarations = Seq(Method, Trait, Interface, Object, Class, Field, Nil)
}

@deriving(JsReader, JsWriter)
sealed trait FileEdit extends Ordered[FileEdit] {
  def file: File
  def text: String
  def from: Int
  def to: Int

  // Required as of Scala 2.11 for reasons unknown - the companion to Ordered
  // should already be in implicit scope
  import scala.math.Ordered.orderingToOrdered

  def compare(that: FileEdit): Int =
    (this.file, this.from, this.to, this.text)
      .compare((that.file, that.from, that.to, that.text))
}

@deriving(JsReader, JsWriter)
final case class TextEdit(file: File, from: Int, to: Int, text: String)
    extends FileEdit

// the next case classes have weird fields because we need the values in the protocol
@deriving(JsReader, JsWriter)
final case class NewFile(file: File, from: Int, to: Int, text: String)
    extends FileEdit
object NewFile {
  def apply(file: File, text: String): NewFile =
    new NewFile(file, 0, text.length - 1, text)
}

@deriving(JsReader, JsWriter)
final case class DeleteFile(file: File, from: Int, to: Int, text: String)
    extends FileEdit
object DeleteFile {
  def apply(file: File, text: String): DeleteFile =
    new DeleteFile(file, 0, text.length - 1, text)
}

@deriving(JsReader, JsWriter)
sealed trait NoteSeverity
@deriving(JsReader, JsWriter)
case object NoteError extends NoteSeverity
@deriving(JsReader, JsWriter)
case object NoteWarn extends NoteSeverity
@deriving(JsReader, JsWriter)
case object NoteInfo extends NoteSeverity
object NoteSeverity {
  def apply(severity: Int) = severity match {
    case 2 => NoteError
    case 1 => NoteWarn
    case 0 => NoteInfo
  }
}

@deriving(JsReader, JsWriter)
sealed abstract class RefactorLocation(val symbol: Symbol)

object RefactorLocation {
  @deriving(JsReader, JsWriter)
  case object QualifiedName extends RefactorLocation('qualifiedName)
  @deriving(JsReader, JsWriter)
  case object File extends RefactorLocation('file)
  @deriving(JsReader, JsWriter)
  case object NewName extends RefactorLocation('newName)
  @deriving(JsReader, JsWriter)
  case object Name extends RefactorLocation('name)
  @deriving(JsReader, JsWriter)
  case object Start extends RefactorLocation('start)
  @deriving(JsReader, JsWriter)
  case object End extends RefactorLocation('end)
  @deriving(JsReader, JsWriter)
  case object MethodName extends RefactorLocation('methodName)
}

@deriving(JsReader, JsWriter)
sealed abstract class RefactorType(val symbol: Symbol)

object RefactorType {
  @deriving(JsReader, JsWriter)
  case object Rename extends RefactorType('rename)
  @deriving(JsReader, JsWriter)
  case object ExtractMethod extends RefactorType('extractMethod)
  @deriving(JsReader, JsWriter)
  case object ExtractLocal extends RefactorType('extractLocal)
  @deriving(JsReader, JsWriter)
  case object InlineLocal extends RefactorType('inlineLocal)
  @deriving(JsReader, JsWriter)
  case object OrganizeImports extends RefactorType('organizeImports)
  @deriving(JsReader, JsWriter)
  case object AddImport extends RefactorType('addImport)
  @deriving(JsReader, JsWriter)
  case object ExpandMatchCases extends RefactorType('expandMatchCases)

  def allTypes =
    Seq(Rename,
        ExtractMethod,
        ExtractLocal,
        InlineLocal,
        OrganizeImports,
        AddImport,
        ExpandMatchCases)
}

/**
 * Represents a source file that has a physical location (either a
 * file or an archive entry) with (optional) up-to-date information in
 * another file, or as a String.
 *
 * Clients using a wire protocol should prefer `contentsIn` for
 * performance (string escaping), whereas in-process clients should
 * use the `contents` variant.
 *
 * If both contents and contentsIn are provided, contents is
 * preferred.
 *
 * Good clients provide the `id` field so the server doesn't have to
 * work it out all the time.
 */
@deriving(JsReader, JsWriter)
final case class SourceFileInfo(
  file: EnsimeFile,
  contents: Option[String] = None,
  contentsIn: Option[File] = None,
  id: Option[EnsimeProjectId] = None
) {
  // keep the log file sane for unsaved files
  override def toString =
    s"SourceFileInfo($file,${contents.map(_ => "...")},$contentsIn)"
}

@deriving(JsReader, JsWriter)
final case class OffsetRange(from: Int, to: Int)

@deprecating("move all non-model code out of the api")
object OffsetRange extends ((Int, Int) => OffsetRange) {
  def apply(fromTo: Int): OffsetRange = new OffsetRange(fromTo, fromTo)
}
