package fpinscala.laziness

import org.scalatest._

class Exercise5_16Spec extends FlatSpec {

  val stream1: Stream[Int] = Stream.cons(3, Empty)
  val stream2: Stream[Int] = Stream.cons(2, stream1)
  val stream3: Stream[Int] = Stream.cons(1, stream2)

  "stream3.scanRight" should "return Stream(stream3, stream2, stream1, empty)" in {
    assert(stream3.scanRight(0)(_ + _).toList === List(6, 5, 3, 0))
  }

  "empty.scanRight" should "return Stream(stream3, stream2, stream1, empty)" in {
    val e = Empty: Stream[Int]
    assert(e.scanRight(0)(_ + _).toList === List(0))
  }
}
