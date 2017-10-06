// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

class HtmlUtilsSpec extends EnsimeSpec {

  "HtmlUtil" should "not unescape plain text" in {
    val originalEscaped = "plain text"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(Some(originalEscaped))
  }

  it should "not unescape empty strings" in {
    val originalEscaped = ""
    HtmlUtil.unescapeHtml(originalEscaped) should ===(Some(originalEscaped))
  }

  it should "unescape ampersands" in {
    val originalEscaped = "Emacs &amp; Ensime"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(Some("Emacs & Ensime"))

  }

  it should "unescape quotes" in {
    val originalEscaped = "&quot;Emacs&quot; &amp; Ensime"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(
      Some("\"Emacs\" & Ensime")
    )

  }

  it should "unescape first character only" in {
    val originalEscaped = "&lt; less than fun without Ensime"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(
      Some("< less than fun without Ensime")
    )
  }

  it should "unescape last character only" in {
    val originalEscaped = "Ensime is greater than all &gt;"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(
      Some("Ensime is greater than all >")
    )

  }

  it should "unescape apostrophes" in {
    val originalEscaped = "Emacs is Ensime&apos;s friend"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(
      Some("Emacs is Ensime's friend")
    )
  }

  "HtmlUtil" should "handle unfinished unescape sequences nicely" in {
    val originalEscaped = "me &amp you"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(None)
  }

  "HtmlUtil" should "handle nested unescape sequences nicely" in {
    val originalEscaped = "me &&amp;; you"
    HtmlUtil.unescapeHtml(originalEscaped) should ===(None)
  }

}
