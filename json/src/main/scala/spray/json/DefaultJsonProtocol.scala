// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

/**
 * Provides all the predefined JsonFormats.
 */
trait DefaultJsonProtocol
    extends BasicFormats
    with StandardFormats
    with CollectionFormats
    with AdditionalFormats

object DefaultJsonProtocol extends DefaultJsonProtocol
