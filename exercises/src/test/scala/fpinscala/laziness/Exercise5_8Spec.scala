package fpinscala.laziness

import org.scalatest._

class Exercise5_8Spec extends FlatSpec {

  "constant(1).take(5).toList" should "return List(1, 1, 1, 1, 1)" in {
    assert(Stream.constant(1).take(5).toList === List(1, 1, 1, 1, 1))
  }

}
