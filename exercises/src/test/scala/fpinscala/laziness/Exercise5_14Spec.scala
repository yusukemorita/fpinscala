package fpinscala.laziness

import org.scalatest._

class Exercise5_14Spec extends FlatSpec {

  val stream1: Stream[Int] = Stream.cons(3, Empty)
  val stream2: Stream[Int] = Stream.cons(2, stream1)
  val stream3: Stream[Int] = Stream.cons(1, stream2)

  "stream(1, 2, 3).startsWith(Stream(1, 2))" should "return true" in {
    val prefixStream = Stream.cons(1, Stream.cons(2, Empty))
    assert(stream3.startsWith(prefixStream) === true)
  }

  "stream(1, 2, 3).startsWith(Stream(2, 3))" should "return false" in {
    assert(stream3.startsWith(stream2) === false)
  }

  "stream(1, 2, 3).startsWith(empty)" should "return false" in {
    assert(stream3.startsWith(Empty) === false)
  }
}
