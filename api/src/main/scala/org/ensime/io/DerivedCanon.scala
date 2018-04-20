// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs/contributors
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package org.ensime.io

import shapeless.{ :: => :*:, _ }
import scalaz.ioeffect.IO

trait DerivedCanon[Repr] {
  def canon(a: Repr): IO[Repr]
}
object DerivedCanon {
  def gen[A, Repr](
    implicit
    G: Generic.Aux[A, Repr],
    CR: Cached[Strict[DerivedCanon[Repr]]]
  ): Canon[A] = a => CR.value.value.canon(G.to(a)).map(G.from)

  implicit val hnil: DerivedCanon[HNil] = _ => IO.now(HNil)
  implicit def hcons[Value, Remaining <: HList](
    implicit LV: Lazy[Canon[Value]],
    DR: DerivedCanon[Remaining]
  ): DerivedCanon[Value :*: Remaining] = {
    case head :*: tail =>
      for {
        h <- LV.value.canon(head)
        t <- DR.canon(tail)
      } yield h :: t
  }

  implicit def cnil: DerivedCanon[CNil] = _ => IO.never
  implicit def ccons[Instance, Remaining <: Coproduct](
    implicit
    LI: Lazy[Canon[Instance]],
    DR: DerivedCanon[Remaining]
  ): DerivedCanon[Instance :+: Remaining] = {
    case Inl(ins) => LI.value.canon(ins).map(Inl(_))
    case Inr(rem) => DR.canon(rem).map(Inr(_))
  }

}
