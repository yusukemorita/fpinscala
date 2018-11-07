package fpinscala.datastructures

import org.scalatest._

class Exercise3_23Spec extends FlatSpec {

  "zipWith(List(1, 2, 3), List(1, 2, 3)(_ * _)" should "return List(1, 4, 9)" in {
    assert(List.zipWith(List(1, 2, 3), List(1, 2, 3))((a: Int, b: Int) => a * b) === List(1, 4, 9))
  }

  "zipWith(List.zipWith(List(1, 2, 3), List(a, b, c)" should "return List(1a, 2b, 3c)" in {
    assert(List.zipWith(List(1, 2, 3), List("a", "b", "c"))((a: Int, b: String) => a + b) === List("1a", "2b", "3c"))
  }
}
