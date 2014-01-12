package org.scalabcn.s99

object Week2 {
  def encodeModified[T](xs: List[T]) = Week1.pack(xs) map { x =>
    if (x.length == 1) x.head
    else (x.length, x.head)
  }
}
