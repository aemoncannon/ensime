// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import org.scalactic.source.Position

trait FormatSpec extends SexpSpec {
  def assertFormat[T: SexpReader: SexpWriter](
    start: T,
    expect: Sexp
  )(implicit p: Position): Unit = {
    val sexp = SexpWriter[T].write(start)
    assert(sexp === expect,
           s"${SexpCompactPrinter(sexp)} was not ${SexpCompactPrinter(expect)}")
    SexpReader[T].read(sexp) should be(start)
  }
}
