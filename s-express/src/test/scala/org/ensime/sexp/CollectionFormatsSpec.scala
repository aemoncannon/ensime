// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.sexp

import collection.{ immutable => im }

// http://docs.scala-lang.org/overviews/collections/overview.html
class CollectionFormatsSpec extends FormatSpec {

  val foo                = SexpString("foo")
  val foos: List[String] = List("foo", "foo")
  val expect             = SexpList(foo, foo)

  "CollectionFormats traits" should "support Traversable" in {
    assertFormat(collection.Traversable[String](), SexpNil)
    assertFormat(collection.Traversable(foos: _*), expect)
  }

  it should "support Iterable" in {
    assertFormat(collection.Iterable[String](), SexpNil)
    assertFormat(collection.Iterable(foos: _*), expect)
  }

  it should "support Seq" in {
    assertFormat(collection.Seq[String](), SexpNil)
    assertFormat(collection.Seq(foos: _*), expect)
  }

  it should "support IndexedSeq" in {
    assertFormat(collection.IndexedSeq[String](), SexpNil)
    assertFormat(collection.IndexedSeq(foos: _*), expect)
  }

  it should "support LinearSeq" in {
    assertFormat(collection.LinearSeq[String](), SexpNil)
    assertFormat(collection.LinearSeq(foos: _*), expect)
  }

  it should "support Set" in {
    assertFormat(collection.Set[String](), SexpNil)
    assertFormat(collection.Set(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support SortedSet" in {
    assertFormat(collection.SortedSet[String](), SexpNil)
    assertFormat(collection.SortedSet(foos: _*), SexpList(foo)) // dupes removed
  }

  "CollectionFormats immutable variants of the traits" should "support Traversable" in {
    assertFormat(im.Traversable[String](), SexpNil)
    assertFormat(im.Traversable(foos: _*), expect)
  }

  it should "support Iterable" in {
    assertFormat(im.Iterable[String](), SexpNil)
    assertFormat(im.Iterable(foos: _*), expect)
  }

  it should "support Seq" in {
    assertFormat(im.Seq[String](), SexpNil)
    assertFormat(im.Seq(foos: _*), expect)
  }

  it should "support IndexedSeq" in {
    assertFormat(im.IndexedSeq[String](), SexpNil)
    assertFormat(im.IndexedSeq(foos: _*), expect)
  }

  it should "support LinearSeq" in {
    assertFormat(im.LinearSeq[String](), SexpNil)
    assertFormat(im.LinearSeq(foos: _*), expect)
  }

  it should "support Set" in {
    assertFormat(im.Set[String](), SexpNil)
    assertFormat(im.Set(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support SortedSet" in {
    assertFormat(im.SortedSet[String](), SexpNil)
    assertFormat(im.SortedSet(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support Map" in {
    assertFormat(im.Map[String, String](), SexpNil)
    assertFormat(im.Map("foo" -> "foo"), SexpList(SexpList(foo, foo)))
  }

  "CollectionFormats immutable specific implementations" should "support im.List" in {
    assertFormat(im.List[String](), SexpNil)
    assertFormat(im.List(foos: _*), expect)
  }

  it should "support im.Vector" in {
    assertFormat(im.Vector[String](), SexpNil)
    assertFormat(im.Vector(foos: _*), expect)
  }

}
