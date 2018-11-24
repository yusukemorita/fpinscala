package fpinscala.laziness

import org.scalatest._

class Exercise5_3Spec extends FlatSpec {

  "stream(1).takeWhile(_ > 0)" should "return stream(1))" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.takeWhile(_ > 0).toList === List(1))
  }

  "stream(2, 1).takeWhile(_ > 1)" should "return stream(2))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.takeWhile(_ > 1).toList === List(2))
  }
}
