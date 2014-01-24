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

  def randomPermute(xs: List[Int]) = randomSelect(xs.length, xs)

  def combinations[T](n: Int, xs: List[T]): List[List[T]] = {
    def inner(n: Int, xs: List[T]): (Boolean, List[List[T]]) = (n, xs) match {
      case (0, _) => (true, List(Nil))
      case (_, Nil) => (false, List(Nil))
      case (_, y :: ys) =>
        val (w_valid, w_result) = inner(n-1, ys)
        val (wo_valid, wo_result) = inner(n, ys)
        (w_valid, wo_valid) match {
          case (true, true) => (true, (w_result map (y :: _)) ::: wo_result)
          case (true, false) => (true, w_result map (y :: _))
          case (false, true) => (true, wo_result)
          case (false, false) => (false, List(Nil))
        }
    }
    val (_, result) = inner(n, xs)
    result
  }
}
