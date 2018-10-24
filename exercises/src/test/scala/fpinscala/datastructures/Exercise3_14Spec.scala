package fpinscala.datastructures

import org.scalatest._

class Exercise3_14Spec extends FlatSpec {

  "append2(List(1, 2), List(3))" should "return List(1, 2, 3)" in {
    assert(List.append2(List(1, 2), List(3)) === List(1, 2, 3))
  }

  "append2(List(1, 2), Nil)" should "return List(1, 2)" in {
    assert(List.append2(List(1, 2), Nil) === List(1, 2))
  }

  "append2(List(1, 2), Nil)" should "return List(1, 2)" in {
    assert(List.append2(List(1, 2), Nil) === List(1, 2))
  }
}
