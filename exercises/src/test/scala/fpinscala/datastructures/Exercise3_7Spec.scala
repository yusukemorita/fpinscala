package fpinscala.datastructures

import org.scalatest._

class Exercise3_7Spec extends FlatSpec {

  "empty list" should "have a length of 0" in {
    assert(List.length(Nil) === 0)
  }

  "List(1, 2, 3)" should "have a length of 3" in {
    assert(List.length(List(1, 2, 3)) === 3)
  }

  "List(apple, orange, banana, kiwi)" should "have a length of 4" in {
    assert(List.length(List("apple", "orange", "banana", "kiwi")) === 4)
  }
}
