package fpinscala.datastructures

import org.scalatest._

class Exercise3_20Spec extends FlatSpec {

  "flatMap(List(1, 2, 3)(i => List(i, i))" should "return List(1, 1, 2, 2, 3, 3)" in {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) === List(1, 1, 2, 2, 3, 3))
  }
}
