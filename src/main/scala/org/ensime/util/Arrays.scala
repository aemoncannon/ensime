package org.ensime.util

object Arrays {

  def splice[T: ClassManifest](a: Array[T], start: Int, end: Int, b: Array[T]): Array[T] = {
    val c = new Array[T](a.size + (b.size - (end - start)))
    System.arraycopy(a, 0, c, 0, start)
    System.arraycopy(b, 0, c, start, b.size)
    System.arraycopy(a, end + 1, c, start + b.size, a.size - end - 1)
    c
  }
}
