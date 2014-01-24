package org.scalabcn.s99

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.PropertyChecks
import org.scalabcn.s99.Week3._
import scala.util.Random

class Week3FlatSpec  extends FlatSpec with Matchers with PropertyChecks {
  "insertAt (P21)" should "insert an element at a given position into a list" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))

    forAll { (left: List[Int], elem: Int, right: List[Int]) =>
      val joined = left ::: elem :: right
      val before = left ::: right
      insertAt(elem, left.length, before) should be (joined)
    }
  }

  "range (P22)" should "create a list containing all integers within a given range" in {
    range(4, 9) should be (List(4, 5, 6, 7, 8, 9))

    forAll { (left: Int, right: Int) =>
      val min = left min right
      val max = (left max right) min (min + 10000)
      range(min, max) should be ((min to max).toList)
    }
  }

  "randomSelect (P23)" should "extract a given number of randomly selected elements from a list" in {
    forAll { (list: List[Int]) =>
      val numElements = Random.nextInt(5) min list.length
      val selected = randomSelect(numElements, list)
      selected.length should be (numElements)
      selected map (list contains _) forall  (_ == true) should be (true)
    }
  }

  "lotto (P24)" should "draw N different random numbers from the set 1..M" in {
    forAll { (max: Int) =>
      val top = if (max == Int.MinValue) 0 else math.abs(max) min 10000
      val numElements = Random.nextInt(10) min top
      println(s"going at it with $top ($max), $numElements")
      val selected = lotto(numElements, top)
      selected.length should be (numElements)
      selected map (x => x > 0 && x <= top) forall  (_ == true) should be (true)
      selected.toSet.size should be (numElements)
    }
  }

  "randomPermute (P25)" should "generate a random permutation of the elements of a list" in {
    forAll { (list: List[Int]) =>
      randomPermute(list).sorted should be (list.sorted)
    }
  }

  "combinations (P26)" should "generate the combinations of K distinct objects chosen from the N elements of a list" in {
    combinations(0, Nil) should be (List(Nil))
    combinations(3, List(1, 2, 3, 4)) should be (List(List(1, 2, 3), List(1, 2, 4), List(1, 3, 4), List(2, 3, 4)))
  }

}
