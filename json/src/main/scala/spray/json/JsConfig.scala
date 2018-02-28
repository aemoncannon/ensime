// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

/**
 * Typehint is sed by the DerivedJs{Reader,Writer} to differentiate between
 * coproducts.
 *
 * Valuehint is used for coproducts values that are not represented by a
 * JsObject. Note that reading such formats is slightly slower and corner cases
 * can exist where roundtripping may fail.
 */
final case class JsConfig[A](typehint: String, valuehint: String)
object JsConfig {
  implicit def default[A]: JsConfig[A] = JsConfig("typehint", "value")
}
