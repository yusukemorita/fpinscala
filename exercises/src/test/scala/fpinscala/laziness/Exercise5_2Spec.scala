package fpinscala.laziness

import org.scalatest._

class Exercise5_2Spec extends FlatSpec {

  // take

  "stream(1).take(1)" should "return stream(1))" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.take(1).toList === stream.toList)
  }

  "stream(2, 1).take(1)" should "return stream(2))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    assert(stream2.take(1).toList === List(2))
  }

  "stream(2, 1).take(3)" should "return stream(2, 1))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    assert(stream2.take(3).toList === List(2, 1))
  }

  "stream(3, 2, 1).take(3)" should "return stream(3, 2, 1))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream3 = Stream.cons(3, stream2)

    assert(stream3.take(3).toList === List(3, 2, 1))
  }

  "empty.take(3)" should "return empty" in {
    assert(Stream.empty.take(3) === Empty)
  }

  // drop

  "stream(2, 1).drop(1)" should "return stream(1))" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    assert(stream2.drop(1).toList === List(1))
  }

  "stream(2, 1).drop(2)" should "return empty)" in {
    val stream1 = Stream.cons(1, Empty)
    val stream2 = Stream.cons(2, stream1)
    assert(stream2.drop(2) === Empty)
  }
}
