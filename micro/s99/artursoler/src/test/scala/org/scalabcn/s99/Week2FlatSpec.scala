package org.scalabcn.s99

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.PropertyChecks
import org.scalabcn.s99.Week2._

class Week2FlatSpec  extends FlatSpec with Matchers with PropertyChecks {
  "encodeModified (P11)" should "be like encode but transforming (1, x) => x" in {
    forAll { (list: List[Boolean]) =>
      val encoded = encodeModified(list)
      ((encoded map {
        case (1, value) => throw new Exception
        case (num: Int, value: Boolean) => List.fill(num)(value)
        case value => value
      }) map {
        case x: List[_] => x
        case x => List(x)
      }).flatten should be (list)
    }
  }
}
