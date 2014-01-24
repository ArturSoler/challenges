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

  def duplicate[T](xs: List[T]) = xs flatMap (List.fill(2)(_))

  def duplicateN[T](n: Int, xs: List[T]) = xs flatMap (List.fill(n)(_))

  def drop[T](n: Int, xs: List[T]) = {
    def inner(counter: Int, xs: List[T]): List[T] = (counter, xs) match {
      case (_, Nil) => Nil
      case (0, x :: xs) => inner(n, xs)
      case (_, x :: xs) => x :: inner(counter - 1, xs)
    }
    inner(n, xs)
  }
}
