package fpinscala.datastructures

import org.scalatest._

class Exercise3_15Spec extends FlatSpec {

  "flatten(Nil)" should "return Nil" in {
    assert(List.flatten(Nil) === Nil)
  }

  "flatten(List(Nil))" should "return Nil" in {
    assert(List.flatten(List(Nil)) === Nil)
  }

  "flatten(List(List(1, 2)))" should "return List(1, 2)" in {
    assert(List.flatten(List(List(1, 2))) === List(1, 2))
  }

  "flatten(List(List(1, 2), List(3)))" should "return List(1, 2, 3)" in {
    assert(List.flatten(List(List(1, 2), List(3))) === List(1, 2, 3))
  }

  "flatten(List(List(1, 2), List(3), List(4, 5, 6)))" should "return List(1, 2, 3, 4, 5, 6)" in {
    assert(List.flatten(List(List(1, 2), List(3), List(4, 5, 6))) === List(1, 2, 3, 4, 5, 6))
  }

  "flatten(List(List(1, 2), List(3), List(4, List(5, 6))))" should "return List(1, 2, 3, 4, List(5, 6))" in {
    assert(List.flatten(List(List(1, 2), List(3), List(4, List(5, 6)))) === List(1, 2, 3, 4, List(5, 6)))
  }

  "flatten(List(List(1, 2, List(3, 4)), List(5)))" should "return List(1, 2, 3, 4, List(5, 6))" in {
    assert(List.flatten(List(List(1, 2, List(3, 4)), List(5))) === List(1, 2, List(3, 4), 5))
  }
}
