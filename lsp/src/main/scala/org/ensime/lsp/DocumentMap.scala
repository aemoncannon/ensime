package org.ensime.lsp

import monix.eval.{ MVar, Task }

import scala.meta.lsp.TextDocumentItem

/**
 * An in memory cache of text documents.
 *
 * @param state A mutable reference to a map of uris to [[TextDocumentItem]]
 */
final class DocumentMap(state: MVar[Map[String, TextDocumentItem]]) {

  /**
   * Gets the [[TextDocumentItem]] for a uri
   * @param uri The location of the text document
   * @return The [[TextDocumentItem]] or [[None]] if no document is present in
   *         the cache
   */
  def get(uri: String): Task[Option[TextDocumentItem]] =
    state.read.map(_.get(uri))

  /**
   * Adds a document to the cache
   * @param uri The location of the text document
   * @param item The [[TextDocumentItem]] to add
   */
  def put(uri: String, item: TextDocumentItem): Task[Unit] =
    modify(_ + (uri -> item))

  /**
   * Removes a document from the cache
   * @param uri The location of the document
   */
  def remove(uri: String): Task[Unit] = modify(_ - uri)

  /**
   * Determines whether or not a document exists
   * @param uri The location of the document
   */
  def contains(uri: String): Task[Boolean] = state.read.map(_.contains(uri))

  private def modify(
    f: Map[String, TextDocumentItem] => Map[String, TextDocumentItem]
  ): Task[Unit] =
    for {
      docs <- state.take
      _    <- state.put(f(docs))
    } yield ()
}

object DocumentMap {

  /** Creates an empty [[DocumentMap]] */
  def empty: Task[DocumentMap] =
    Task(MVar(Map.empty[String, TextDocumentItem])).map(new DocumentMap(_))
}
