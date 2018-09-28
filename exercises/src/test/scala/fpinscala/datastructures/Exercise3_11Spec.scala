package fpinscala.datastructures

import org.scalatest._

class Exercise3_11Spec extends FlatSpec {

  "sum(List(1)" should "return 1" in {
    assert(List.sum(List(1)) === 1)
  }

  "sum(List(1, 2, 3)" should "return 6" in {
    assert(List.sum(List(1, 2, 3)) === 6)
  }

  "product(List(1)" should "return 1" in {
    assert(List.product(List(1)) === 1)
  }

  "product(List(1, 2, 4)" should "return 8" in {
    assert(List.product(List(1, 2, 4)) === 8)
  }

  "product(List(1, 2, 0)" should "return 0" in {
    assert(List.product(List(1, 2, 0)) === 0)
  }

  "length(List(1, 2, 0)" should "return 3" in {
    assert(List.length(List(1, 2, 0)) === 3)
  }

  "length(Nil)" should "return 0" in {
    assert(List.length(Nil) === 0)
  }

  "length(List(hi, hi, hi, hi, hi, hi, hi))" should "return 7" in {
    assert(List.length(List("hi", "hi", "hi", "hi", "hi", "hi", "hi")) === 7)
  }
}
