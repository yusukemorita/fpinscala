package fpinscala.laziness

import org.scalatest._

class Exercise5_15Spec extends FlatSpec {

  val stream1: Stream[Int] = Stream.cons(3, Empty)
  val stream2: Stream[Int] = Stream.cons(2, stream1)
  val stream3: Stream[Int] = Stream.cons(1, stream2)

  "stream3.tails" should "return Stream(stream3, stream2, stream1, empty)" in {
    assert(stream3.tails.toList.map(_.toList) === List(List(1, 2, 3), List(2, 3), List(3), Nil))
  }

  "empty.tails" should "return Stream(empty)" in {
    assert(Empty.tails.toList === List(Empty))
  }

}
