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
    forAll { (list: List[Int]) =>
      combinations(0, list) should be (List(Nil))
//      combinations(1, list) should be (list map (List(_)))
    }
  }

  "group (P27)" should "group the elements of a set into disjoint subsets" in {
    forAll { (list: List[Int] ) =>
      if (list.length > 5 && list.toSet.size == list.length) {
        val sizes = List(2, 3, list.length-5)
        val groups = group(sizes, list)
        groups map (_.length) forall (_ == 3) should be (true)
        groups map (_.flatten) map (_.length) forall (_ == list.length) should be (true)
        groups map (group => group map (_.toSet)) map (group => (group.head /: group)(_ union _)) forall (_ == list.toSet) should be (true)
      }
    }
  }

  "lsort (P28)" should "sort a list of lists according to length of sublists, in increasing order" in {
    val input = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val output = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    lsort(input) should be (output)
  }

  "lsortFreq (P28)" should "sort a list of lists according to frequency of length of sublists" in {
    val input = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val output = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    lsortFreq(input) should be (output)
  }



}
