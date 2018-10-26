package fpinscala.datastructures

import org.scalatest._

class Exercise3_18Spec extends FlatSpec {

  "map(List(1, 2, 3)(_ + 1)" should "return List(2, 3, 4)" in {
    assert(List.map(List(1, 2, 3))(_ + 1) === List(2, 3, 4))
  }

  "map(List(1.0, 2.3, 4.9)(toString)" should "return List('1.0', '2.3', '4.9')" in {
    assert(List.map(List(1.0, 2.3, 4.9))(_.toString) === List("1.0", "2.3", "4.9"))
  }
}
