package fpinscala.datastructures

import org.scalatest._

class Exercise3_12Spec extends FlatSpec {

  "reverse(List(1, 2))" should "return List(2, 1)" in {
    assert(List.reverse(List(1, 2)) === List(2, 1))
  }

  "reverse(Nil)" should "return Nil" in {
    assert(List.reverse(Nil) === Nil)
  }

  "reverse(List(dog, cat, mouse))" should "return List(mouse, cat, dog)" in {
    assert(List.reverse(List("dog", "cat", "mouse")) === List("mouse", "cat", "dog"))
  }
}
