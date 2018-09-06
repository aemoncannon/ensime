// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
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

/**
 * Wraps requests to the Ensime project actor in [[Task]].  These can then be lifted into a larger [[Task]] sequence, and ultimately into an LSP service.
 */
final case class EnsimeProjectWrapper(project: ActorRef) {

  /**
   * Tell the ensime project actor to remove a document from its internal state.  This is called when the user closes a document.
   */
  def removeFile(uri: String): Task[Unit] =
    Task.eval(new File(new URI(uri))).map { file =>
      project ! RemoveFileReq(file)
    }

  /**
   * Tell the ensime project actor to typecheck a document.  This is called whenever a document is modified.
   *
   * @param uri  The uri corresponding to the document's location
   * @param text The text content of the document.  This may not be the text content that is saved on the filesystem as the document may be being edited by the user.
   */
  def typecheckFile(uri: String, text: String): Task[Unit] =
    toSourceFileInfo(uri, text).map { sourceFileInfo =>
      project ! TypecheckFileReq(sourceFileInfo)
    }

  def typecheckFile(path: Path, text: String): Task[Unit] =
    Task(project ! TypecheckFileReq(SourceFileInfo(RawFile(path), Some(text))))

  /**
   * Ask the ensime project actor for the signature of the thing at the specified position.  This is called whenever a user hovers over a thing with the intent to view its signature.
   *
   * @param uri  The uri corresponding to the document containing the thing.
   * @param text The text content of the document containing the thing.
   * @param position  The position of the thing within the text
   */
  def getDocUriAtPoint(uri: String, text: String, position: Position)(
    implicit T: Timeout
  ): Task[Option[String]] =
    for {
      sourceFileInfo <- toSourceFileInfo(uri, text)
      offset <- fromEither(
                 positionToOffset(uri, text, position).left
                   .map(_.toIllegalArgumentException)
               )
      offsetRange = OffsetRange(offset)
      result <- Task.fromFuture(
                 project ? DocUriAtPointReq(Right(sourceFileInfo), offsetRange)
               )
    } yield
      result match {
        case pair @ Some(DocSigPair(DocSig(_, scalaSig), DocSig(_, javaSig))) =>
          Some(scalaSig.orElse(javaSig).getOrElse(""))
        case None => None
      }

  /**
   * Ask the ensime project actor for code completions of the thing at the specified position.
   *
   * @param uri The uri corresponding to the location of the document containing the thing.
   * @param text The text content of the document containing the thing.
   * @param position The position of the thing within the text
   * @return A list of possible completions of the thing.  At most 100 results are returned. Completions are case-insensitive.
   */
  def getCompletions(uri: String, text: String, position: Position)(
    implicit T: Timeout
  ): Task[List[CompletionItem]] =
    for {
      sourceFileInfo <- toSourceFileInfo(uri, text)
      offset <- fromEither(
                 positionToOffset(uri, text, position).left
                   .map(_.toIllegalArgumentException)
               )
      result <- Task.fromFuture(
                 project ? CompletionsReq(
                   sourceFileInfo,
                   offset,
                   maxResults = 100,
                   caseSens = false,
                   // I'm not sure what this does - the value of "reload" is unimportant to the project (see [[Analyzer.allTheThings]])
                   reload = false
                 )
               )
    } yield
      result match {
        case CompletionInfoList(prefix, completions) =>
          // Shouldn't these already be sorted?  After all, we're imposing a cap on the maximum allowed results
          completions
            .sortBy(-_.relevance)
            .map(TextDocumentServices.toCompletion)
      }

  /**
   * Asks the ensime project actor for the structure view of a document.  The structure view members are converted into a flat list of [[SymbolInformation]].
   *
   * @param uri  The uri corresponding to the document's location
   * @param text The text corresponding to the document
   * @return A flat list of all [[SymbolInformation]] in the document's structure.
   */
  def getSymbolInformation(uri: String, text: String)(
    implicit T: Timeout
  ): Task[List[SymbolInformation]] =
    toSourceFileInfo(uri, text).flatMap { sourceFileInfo =>
      Task.fromFuture(project ? StructureViewReq(sourceFileInfo))
    }.map {
      case StructureView(members) =>
        members.flatMap(EnsimeProjectWrapper.toSymbolInformation(uri, text, _))
    }

  /**
   * Gets the Ensime [[SymbolInfo]] of a thing at a point.
   *
   * @param uri The uri of the document containing the thing
   * @param text The text of the document containing the thing
   * @param position The position of the thing
   * @return The [[SymbolInfo]] of the thing.
   */
  def getSymbolAtPoint(uri: String, text: String, position: Position)(
    implicit T: Timeout
  ): Task[SymbolInfo] =
    for {
      sourceFileInfo <- toSourceFileInfo(uri, text)
      offset <- fromEither(
                 positionToOffset(uri, text, position).left
                   .map(_.toIllegalArgumentException)
               )
      result <- Task.fromFuture(
                 project ? SymbolAtPointReq(Right(sourceFileInfo), offset)
               )
    } yield result.asInstanceOf[SymbolInfo]

  /**
   * Convers an [[Either]] into a [[Task]].  This should be included in the most recent version of Monix
   */
  private def fromEither[A](either: Either[Throwable, A]): Task[A] =
    either match {
      case Left(err) => Task.raiseError(err)
      case Right(a)  => Task(a)
    }

  /**
   * Creates a [[SourceFileInfo]] from a document.
   *
   * @param uri  The URI string corresponding to the document's location.
   * @param text The text content of the document.
   * @return     The [[SourceFileInfo]] corresponding to the document
   */
  private def toSourceFileInfo(
    uri: String,
    text: String
  ): Task[SourceFileInfo] =
    // Catch whatever exceptions may occur when resolving the uri
    Task
      .eval((new File(new URI(uri))).toPath)
      .map(filepath => SourceFileInfo(RawFile(filepath), Some(text)))

  /**
   * Calculates the index of a character from its [[Position]]
   *
   * @param uri  The uri corresponding to the document's location.  This is used for logging only.
   * @param text The text content of the document containing the character.
   * @param position  The position of the character within the document.  The position must be contained within the lines and columns of the document.
   * @return An integer index corresponding to the index of the character within the text, or an [[InvalidPosition]] if the position does not exist within the document
   */
  private def positionToOffset(
    uri: String,
    text: String,
    position: Position
  ): Either[EnsimeProjectWrapper.InvalidPosition, Int] =
    Right(TextDocumentServices.positionToOffset(text.toArray, position, uri))
  // val contents = text.toArray
  // val line = position.line
  // val lineNumber = line + 1
  // val col = position.character
  // val columnNumber = col + 1

  // // Could use a negative look behind to split the string while retaining delimiter
  // val lineSeparatorRegex = "\r\n"
  // val lines = text.split(s"(?<=$lineSeparatorRegex)")
  // if(lineNumber > lines.length) {
  //   // The line number doesn't exist in the file
  //   Left(EnsimeProjectWrapper.PositionExceedsNumberOfLines(uri, position, lines.length))
  // } else {
  //   val linesIncludingPosition = lines.take(lineNumber)
  //   val targetLine = linesIncludingPosition.last

  //   if(columnNumber > targetLine.length) {
  //     // The column doesn't exist in the file
  //     Left(EnsimeProjectWrapper.PositionExceedsNumberOfCharacters(uri, position, targetLine))
  //   } else {
  //     val charactersIncludingPosition = targetLine.take(columnNumber)
  //     val textIncludingPosition = linesIncludingPosition.dropRight(1).mkString("") + charactersIncludingPosition
  //     Right(textIncludingPosition.length - 1)
  //   }
  // }
}

object EnsimeProjectWrapper {
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

  /**
   * Converts an ensime [[CompletionInfo]] into an LSP [[CompletionItem]].
   *
   * LSP completion items are language generic, so some detail is lost here.
   */
  def toCompletion(completionInfo: CompletionInfo): CompletionItem = {
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
  def toSymbolInformation(
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
          TextDocumentServices.offsetToPosition(uri, text.toArray, offset)
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
}
