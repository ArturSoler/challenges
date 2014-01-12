package org.scalabcn.s99

object Week2 {
  def encodeModified[T](xs: List[T]) = Week1.pack(xs) map { x =>
    if (x.length == 1) x.head
    else (x.length, x.head)
  }

  def decode[T](xs: List[(Int, T)]) = (xs map {
    case (num, value) => List.fill(num)(value)
  }).flatten

  def encodeDirect[T](xs: List[T]): List[(Int, T)] = xs match {
    case Nil => Nil
    case x =>
      val (take, drop) = xs.span(_ == x.head)
      (take.length, take.head) :: encodeDirect(drop)
  }
}
