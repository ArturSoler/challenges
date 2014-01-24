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

  "drop (P16)" should "drop every nth element in the input list" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "split (P17)" should "split the input list in 2 lists, the first one being as long as indicated by the parameter" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    forAll { (left: List[Int], right: List[Int]) =>
      val joined = left ::: right
      split(left.length, joined) should be (left, right)
    }
  }

  "slice (P18)" should "produce a slice of the input list with the given indexes" in {
    Week2.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('d, 'e, 'f, 'g)

    forAll { (left: List[Int], slice: List[Int], right: List[Int]) =>
      val joined = left ::: slice ::: right
      Week2.slice(left.length, left.length+slice.length, joined) should be (slice)
    }
  }

  "rotate (P19)" should "rotate a list the given number of elements" in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

    forAll { (left: List[Int], right: List[Int]) =>
      val joined = left ::: right
      val rotated = right ::: left
      rotate(left.length, joined) should be (rotated)
      rotate(-right.length, joined) should be (rotated)
    }
  }

  "removeAt (P20)" should "return the list and the removed element in a tuple" in {
    removeAt(1, List('a, 'b, 'c, 'd)) should be (List('a, 'c, 'd),'b)

    forAll { (left: List[Int], elem: Int, right: List[Int]) =>
      val joined = left ::: elem :: right
      val removed = left ::: right
      removeAt(left.length, joined) should be (removed, elem)
    }
  }
}
