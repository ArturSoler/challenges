package org.scalabcn.s99

import scala.util.Random

object Week3 {
  def insertAt[T](elem: T, pos: Int, xs: List[T]): List[T] = pos match {
    case 0 => elem :: xs
    case _ => xs.head :: insertAt(elem, pos-1, xs.tail)
  }

  def range(left: Int, right: Int) = {
    def inner(count: Int, accum: List[Int]): List[Int] = if(count == left) count :: accum else inner(count-1, count :: accum)
    if (left <= right)
      inner(right, Nil)
    else
      Nil
  }

  def randomSelect[T](n: Int, xs: List[T]) = Random.shuffle(xs) take n

  def lotto(num: Int, top: Int) = randomSelect(num, range(1, top))
}
