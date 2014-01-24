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

  def split[T](n: Int, xs: List[T]): (List[T], List[T]) = n match {
    case 0 => (Nil, xs)
    case _ =>
      val (first, second) = split(n-1, xs.tail)
      (xs.head :: first, second)
  }

  def slice[T](start: Int, end: Int, xs: List[T]): List[T] = (start, end, xs) match { // xs.take(end).drop(start)
    case (0, 0, _) => Nil
    case (0, _, x :: xs) => x :: slice(0, end - 1, xs)
    case (_, _, _ :: xs) => slice(start-1, end-1, xs)
    case (_, _, Nil) => Nil
  }

  def rotate[T](n: Int, xs: List[T]): List[T] =
    if (n >= 0) xs.drop(n) ::: xs.take(n)
    else xs.takeRight(-n) ::: xs.dropRight(-n)
}
