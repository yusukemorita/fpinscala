package fpinscala.laziness

import org.scalatest._

class Exercise5_5Spec extends FlatSpec {

  "stream(2, 1).takeWhileByFoldRight(_ > 0)" should "return stream(2, 1))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.takeWhileByFoldRight(_ > 0).toList === List(2, 1))
  }

  "stream(2, 1).takeWhileByFoldRight(_ > 1)" should "return stream(2))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.takeWhileByFoldRight(_ > 1).toList === List(2))
  }

  "stream(2, 1).takeWhileByFoldRight(_ > 3)" should "return empty)" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.takeWhileByFoldRight(_ > 3) === Empty)
  }
}
