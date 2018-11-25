package fpinscala.laziness

import org.scalatest._

class Exercise5_10Spec extends FlatSpec {

  "fibs.take(5).toList" should "return List(0, 1, 1, 2, 3)" in {
    assert(Stream.fibs.take(5).toList === List(0, 1, 1, 2, 3))
  }

}
