package fpinscala.laziness

import org.scalatest._

class Exercise5_12Spec extends FlatSpec {

  // fibs

  "fibs.take(5).toList" should "return List(0, 1, 1, 2, 3)" in {
    assert(Stream.fibsByUnfold.take(6).toList === List(0, 1, 1, 2, 3, 5))
  }

  // from

  "from(3).take(5).toList" should "return List(0, 1, 2, 3, 4)" in {
    assert(Stream.fromByUnfold(3).take(5).toList === List(3, 4, 5, 6, 7))
  }

  // constant

  "constant(3).take(5).toList" should "return List(0, 1, 2, 3, 4)" in {
    assert(Stream.constantByUnfold(3).take(5).toList === List(3, 3, 3, 3, 3))
  }
}
