package fpinscala.laziness

import org.scalatest._

class Exercise5_9Spec extends FlatSpec {

  "from(1).take(5).toList" should "return List(1, 2, 3, 4, 5)" in {
    assert(Stream.from(1).take(5).toList === List(1, 2, 3, 4, 5))
  }

}
