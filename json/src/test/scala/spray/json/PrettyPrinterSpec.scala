// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package spray.json

import scala.collection.immutable.ListMap
import org.scalatest._
import Matchers._

class PrettyPrinterSpec extends WordSpec {

  "The PrettyPrinter" should {
    "print a more complicated JsObject nicely aligned" in {
      val JsObject(fields) = JsParser {
        """{
          |  "Boolean no": false,
          |  "Boolean yes":true,
          |  "Unic\u00f8de" :  "Long string with newline\nescape",
          |  "key with \"quotes\"" : "string",
          |  "key with spaces": null,
          |  "number": -1.2323424E-5,
          |  "simpleKey" : "some value",
          |  "sub object" : {
          |    "sub key": 26.5,
          |    "a": "b",
          |    "array": [1, 2, { "yes":1, "no":0 }, ["a", "b", null], false]
          |  },
          |  "zero": 0
          |}""".stripMargin
      }
      PrettyPrinter(JsObject(ListMap(fields.toSeq.sortBy(_._1): _*))) shouldEqual {
        """{
          |  "Boolean no": false,
          |  "Boolean yes": true,
          |  "Unic\u00f8de": "Long string with newline\nescape",
          |  "key with \"quotes\"": "string",
          |  "key with spaces": null,
          |  "number": -0.000012323424,
          |  "simpleKey": "some value",
          |  "sub object": {
          |    "sub key": 26.5,
          |    "a": "b",
          |    "array": [1, 2, {
          |      "yes": 1,
          |      "no": 0
          |    }, ["a", "b", null], false]
          |  },
          |  "zero": 0
          |}""".stripMargin
      }
    }
  }

}
