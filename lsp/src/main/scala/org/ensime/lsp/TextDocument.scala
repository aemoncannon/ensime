// // Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// // License: http://www.gnu.org/licenses/gpl-3.0.en.html
// package org.ensime.lsp

// import java.io.File
// import java.net.URI

// import akka.util.Timeout
// import scala.concurrent.duration._

// object TextDocument {

//   implicit val timeout: Timeout     = Timeout(5 seconds)

//   def positionToOffset(contents: Array[Char], pos: scala.meta.lsp.Position, uri: String): Int = {
//     val line = pos.line
//     val col = pos.character

//     var i = 0
//     var l = 0

//     while (i < contents.length && l < line) {
//       contents(i) match {
//         case '\r' =>
//           l += 1
//           if (peek(i + 1, contents) == '\n') i += 1

//         case '\n' =>
//           l += 1

//         case _ =>
//       }
//       i += 1
//     }

//     if (l < line)
//       throw new IllegalArgumentException(
//         s"$uri: Can't find position $pos in contents of only $l lines long."
//       )
//     if (i + col < contents.length)
//       i + col
//     else
//       throw new IllegalArgumentException(
//         s"$uri: Invalid column. Position $pos in line '${contents.slice(i, contents.length).mkString}'"
//       )
//   }

//   /**
//    * Return the corresponding position in this text document as 0-based line and column.
//    */
//   def offsetToPosition(uri: String, contents: Array[Char], offset: Int): scala.meta.lsp.Position = {
//     if (offset >= contents.length)
//       throw new IndexOutOfBoundsException(
//         s"$uri: asked position at offset $offset, but contents is only ${contents.length} characters long."
//       )

//     var i    = 0
//     var line = 0
//     var col  = 0

//     while (i < offset) {
//       contents(i) match {
//         case '\r' =>
//           line += 1
//           col = 0
//           if (peek(i + 1, contents) == '\n') i += 1

//         case '\n' =>
//           line += 1
//           col = 0

//         case _ =>
//           col += 1
//       }
//       i += 1
//     }

//     scala.meta.lsp.Position(line, col)
//   }

//   private[this] def peek(idx: Int, contents: Array[Char]): Int =
//     if (idx < contents.length) contents(idx).toInt else -1
// }
