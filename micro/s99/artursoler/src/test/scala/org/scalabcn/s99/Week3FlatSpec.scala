package org.scalabcn.s99

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.PropertyChecks
import org.scalabcn.s99.Week3._

class Week3FlatSpec  extends FlatSpec with Matchers with PropertyChecks {
  "insertAt (P20)" should "insert an element at a given position into a list" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))

    forAll { (left: List[Int], elem: Int, right: List[Int]) =>
      val joined = left ::: elem :: right
      val before = left ::: right
      insertAt(elem, left.length, before) should be (joined)
    }
  }
}
