package org.scalabcn.s99

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.PropertyChecks
import org.scalabcn.s99.Week3._

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
}
