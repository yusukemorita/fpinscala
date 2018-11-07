package fpinscala.datastructures

import org.scalatest._

class Exercise3_21Spec extends FlatSpec {

  "filterWithFlatMap(List(1, 2, 3)(_ % 2 == 1)" should "return List(1, 3)" in {
    assert(List.filterWithFlatMap(List(1, 2, 3))(_ % 2 == 1) === List(1, 3))
  }

  "filterWithFlatMap(List(hi, hello, goodbye)(_.size > 3)" should "return List(hello, goodbye)" in {
    assert(List.filterWithFlatMap(List("hi", "hello", "goodbye"))(_.length  > 3) === List("hello", "goodbye"))
  }
}
