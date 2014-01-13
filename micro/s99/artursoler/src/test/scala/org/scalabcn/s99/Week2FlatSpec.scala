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

  "decode (P12)" should "decode the result of encode (P10)" in {
    forAll { (list: List[Boolean]) =>
      decode(Week1.encode(list)) should be (list)
    }
  }

  "encodeDirect (P13)" should "behave as P10" in {
    forAll { (list: List[Boolean]) =>
      encodeDirect(list) should be (Week1.encode(list))
    }
  }

  "duplicate (P14)" should "duplicate each entry in the original list" in {
    forAll { (list: List[Int]) =>
      def deduplicate[T](xs: List[T]): List[T] = xs match {
        case Nil => Nil
        case y1 :: y2 :: ys if (y1 == y2) => y1 :: deduplicate(ys)
        case _ => throw new IllegalArgumentException
      }

      deduplicate(duplicate(list)) should be (list)
    }
  }

  "duplicateN (P15)" should "duplicate each entry in the list N times" in {
    forAll { (list: List[Int], n: Int) =>
      if(n > 0 && n < 10000)
      {
        def deduplicateN[T](xs: List[T]): List[T] = xs match {
          case Nil => Nil
          case _ =>
            val (step, rest) = xs splitAt n
            step.takeWhile(_ == step.head) should be (step)
            step.head :: deduplicateN(rest)
        }

        deduplicateN(duplicateN(n, list)) should be (list)
      }
    }
  }
}
