package org.ensime.lsp

import java.io.File
import java.nio.file.Path
import java.net.URI

import akka.actor.ActorRef
import akka.util.Timeout
import akka.pattern.ask
import org.ensime.api._
import org.ensime.core.{ DocSig, DocSigPair }
import monix.eval.Task
import scala.meta.lsp.{
  CompletionItem,
  CompletionItemKind,
  Location,
  Position,
  Range,
  SymbolInformation,
  SymbolKind
}

object EnsimeToLspAdapter {

  /** Map of keywords to [[SymbolKind]] used to convert symbol information */
  private val KeywordToKind: Map[String, SymbolKind] = Map(
    "class"   -> SymbolKind.Class,
    "trait"   -> SymbolKind.Interface,
    "type"    -> SymbolKind.Interface,
    "package" -> SymbolKind.Package,
    "def"     -> SymbolKind.Method,
    "val"     -> SymbolKind.Constant,
    "var"     -> SymbolKind.Field
  )

  /**
   * Converts an ensime [[StructureViewMember]] into an LSP [[SymbolInformation]].
   *
   * The member is traversed recursively, with each child member being converted into a [[SymbolInformation]].
   *
   * @param uri The uri corresponding to the document containing the member
   * @param text The text of the document containing the member
   * @param structure The member to extract [[SymbolInformation]] from
   * @return a flat list of the [[SymbolInformation]] of the member and all of its child members.
   */
  def structureViewMemberToSymbolInformation(
    uri: String,
    text: String,
    structure: StructureViewMember
  ): List[SymbolInformation] = {

    // recurse over the child members of the member
    def go(member: StructureViewMember,
           outer: Option[String]): List[SymbolInformation] = {
      // Why do we default to field?
      val kind = KeywordToKind.getOrElse(member.keyword, SymbolKind.Field)
      val rest = member.members.flatMap(go(_, Some(member.name)))
      val position = member.position match {
        case OffsetSourcePosition(_, offset) =>
          EnsimeToLspAdapter.offsetToPosition(uri, text.toArray, offset)
        case LineSourcePosition(_, line) => Position(line, 0)
        case EmptySourcePosition()       =>
          // The source is empty.  What does this mean?
          Position(0, 0)
      }

      SymbolInformation(
        member.name,
        kind,
        Location(uri,
                 Range(position,
                       position.copy(
                         character = position.character + member.name.length
                       ))),
        outer
      ) :: rest
    }

    go(structure, None)
  }

  /**
   * Converts an ensime [[CompletionInfo]] into an LSP [[CompletionItem]].
   *
   * LSP completion items are language generic, so some detail is lost here.
   */
  def completionInfoToCompletionItem(
    completionInfo: CompletionInfo
  ): CompletionItem = {
    val kind: Option[CompletionItemKind] = completionInfo.typeInfo.map { info =>
      info.declaredAs match {
        case DeclaredAs.Method => CompletionItemKind.Method
        case DeclaredAs.Class  => CompletionItemKind.Class
        case DeclaredAs.Field  => CompletionItemKind.Field
        case DeclaredAs.Interface | DeclaredAs.Trait =>
          CompletionItemKind.Interface
        case DeclaredAs.Object => CompletionItemKind.Module
        // Why do we map everything with a nil type to a value?
        case DeclaredAs.Nil => CompletionItemKind.Value
      }
    }

    CompletionItem(
      label = completionInfo.name,
      kind = kind,
      detail = completionInfo.typeInfo.map(_.fullName)
    )
  }

  /**
   * Return the corresponding position in this text document as 0-based line and column.
   */
  def offsetToPosition(uri: String,
                       contents: Array[Char],
                       offset: Int): scala.meta.lsp.Position = {
    if (offset >= contents.length)
      throw new IndexOutOfBoundsException(
        s"$uri: asked position at offset $offset, but contents is only ${contents.length} characters long."
      )

    var i    = 0
    var line = 0
    var col  = 0

    while (i < offset) {
      contents(i) match {
        case '\r' =>
          line += 1
          col = 0
          if (peek(i + 1, contents) == '\n') i += 1

        case '\n' =>
          line += 1
          col = 0

        case _ =>
          col += 1
      }
      i += 1
    }

    scala.meta.lsp.Position(line, col)
  }

  private[this] def peek(idx: Int, contents: Array[Char]): Int =
    if (idx < contents.length) contents(idx).toInt else -1
}

object LspToEnsimeAdapter {

  /**
   * Calculates the index of a character from its [[Position]]
   *
   * @param uri  The uri corresponding to the document's location.  This is used for logging only.
   * @param text The text content of the document containing the character.
   * @param position  The position of the character within the document.  The position must be contained within the lines and columns of the document.
   * @return An integer index corresponding to the index of the character within the text, or an [[InvalidPosition]] if the position does not exist within the document
   */
  def positionToOffset(
    uri: String,
    text: String,
    position: scala.meta.lsp.Position
  ): Either[InvalidPosition, Int] = {

    val newlineRegex = "(?<=\r?\n)"
    val lines = text.split(newlineRegex)

    if (lines.length <= position.line) {
      Left(PositionExceedsNumberOfLines(uri, position, lines.length))
    } else {
      val line = lines(position.line)

      if(line.length <= position.character) {
        Left(PositionExceedsNumberOfCharacters(uri, position, line))
      } else {
        val previousLinesCharacterCount = lines.take(position.line).map(_.length).sum
        val currentLineCharacterCount = position.character
        Right(previousLinesCharacterCount + currentLineCharacterCount)
      }
    }
  }


  private[this] def peek(idx: Int, contents: Array[Char]): Int =
    if (idx < contents.length) contents(idx).toInt else -1

  /**
   * Creates a [[SourceFileInfo]] from a document.
   *
   * @param uri  The URI string corresponding to the document's location.
   * @param text The text content of the document.
   * @return     The [[SourceFileInfo]] corresponding to the document
   */
  def toSourceFileInfo(
    uri: String,
    text: String
  ): Task[SourceFileInfo] =
    // Catch whatever exceptions may occur when resolving the uri
    Task
      .eval((new File(new URI(uri))).toPath)
      .map(filepath => SourceFileInfo(RawFile(filepath), Some(text)))

  sealed trait InvalidPosition {
    def toIllegalArgumentException: IllegalArgumentException = this match {
      case PositionExceedsNumberOfLines(uri, position, numberOfLines) =>
        new IllegalArgumentException(
          s"$uri: Can't find position $position in contents of only $numberOfLines lines long."
        )
      case PositionExceedsNumberOfCharacters(uri, position, line) =>
        new IllegalArgumentException(
          s"$uri: Invalid column. Position $position in line '$line'"
        )
    }
  }
  case class PositionExceedsNumberOfLines(uri: String,
                                          position: Position,
                                          numberOfLines: Int)
      extends InvalidPosition
  case class PositionExceedsNumberOfCharacters(uri: String,
                                               position: Position,
                                               line: String)
      extends InvalidPosition

}
