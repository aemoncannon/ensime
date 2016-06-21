package org.ensime.util

trait Show[T] {

  def apply(in: T): String

}

object Show {

  def show[A](f: A => String): Show[A] = new Show[A] {
    def apply(a: A): String = f(a)
  }

  def fromToString[A]: Show[A] = new Show[A] {
    def apply(a: A): String = a.toString
  }
}
