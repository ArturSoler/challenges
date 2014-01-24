package org.scalabcn.s99

object Week3 {
  def insertAt[T](elem: T, pos: Int, xs: List[T]): List[T] = pos match {
    case 0 => elem :: xs
    case _ => xs.head :: insertAt(elem, pos-1, xs.tail)
  }
}
