package fpinscala.datastructures

import org.scalatest._

class Exercise3_22Spec extends FlatSpec {

  "addElements(List(1, 2, 3), List(1)" should "return List(2)" in {
    assert(List.addElements(List(1, 2, 3), List(1)) === List(2))
  }

  "addElements(List(1, 2, 3), List(1, 1, 1)" should "return List(2, 3, 4)" in {
    assert(List.addElements(List(1, 2, 3), List(1, 1, 1)) === List(2, 3, 4))
  }

  "addElements(List(1, 2, 3), Nil" should "return List(1, 2, 3)" in {
    assert(List.addElements(List(1, 2, 3), Nil) === Nil)
  }

  "addElements(Nil, List(1, 1, 1)" should "return List(1, 1, 1)" in {
    assert(List.addElements(Nil, List(1, 1, 1)) === Nil)
  }

  "addElements(Nil, Nil)" should "return Nil" in {
    assert(List.addElements(Nil, Nil) === Nil)
  }
}
