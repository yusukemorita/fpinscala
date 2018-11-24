package fpinscala.laziness

import org.scalatest._

class Exercise5_7Spec extends FlatSpec {

  // map

  "stream(1, 2, 3).map(_ * 2)" should "return stream(2, 4, 6)" in {
    val stream1 = Stream.cons(3, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream3 = Stream.cons(1, stream2)
    assert(stream3.map(_ * 2).toList === List(2, 4, 6))
  }

  "stream(1).map(_ + 1)" should "return stream(2)" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.map(_ + 1).toList === List(2))
  }

  "empty.map(_ + 1)" should "return empty" in {
    val stream = Empty: Stream[Int]
    assert(stream.map(_ + 1) === Empty)
  }

  // filter

  "stream(1, 2, 3).filter(_ % 2 == 0)" should "return stream(2)" in {
    val stream1 = Stream.cons(3, Empty)
    val stream2 = Stream.cons(2, stream1)
    val stream3 = Stream.cons(1, stream2)
    assert(stream3.filter(_ % 2 == 0).toList === List(2))
  }

  "empty.filter(_ % 2 == 0)" should "return empty" in {
    val stream = Empty: Stream[Int]
    assert(stream.filter(_ % 2 == 0) === Empty)
  }

  // append

  "stream(1, 2).append(stream(3, 4))" should "return stream(1, 2, 3, 4)" in {
    val stream1 = Stream.cons(1, Stream.cons(2, Empty))
    val stream2 = Stream.cons(3, Stream.cons(4, Empty))
    assert(stream1.append(stream2).toList === List(1, 2, 3, 4))
  }

  "stream(1, 2).append(empty)" should "return stream(1, 2)" in {
    val stream1 = Stream.cons(1, Stream.cons(2, Empty))
    val stream2 = Empty
    assert(stream1.append(stream2).toList === List(1, 2))
  }

  // flatMap

  "stream(stream(1, 2), stream(3, 4)).flatMap(_)" should "return stream(1, 2, 3, 4)" in {
    val stream1 = Stream.cons(1, Stream.cons(2, Empty))
    val stream2 = Stream.cons(3, Stream.cons(4, Empty))
    val stream: Stream[Stream[Int]] = Stream.cons(stream1, Stream.cons(stream2, Empty))
    assert(stream.flatMap(a => a).toList === List(1, 2, 3, 4))
  }

}
