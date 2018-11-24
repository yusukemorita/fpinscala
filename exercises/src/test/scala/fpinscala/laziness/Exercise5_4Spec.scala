package fpinscala.laziness

import org.scalatest._

class Exercise5_4Spec extends FlatSpec {

  "stream(2, 1).forAll(_ > 0)" should "return true)" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.forAll(_ > 0) === true)
  }

  "stream(2, 1).forAll(_ > 1)" should "return false)" in {
    val stream1 = Stream.cons(1, Empty)
    val stream = Stream.cons(2, stream1)
    assert(stream.forAll(_ > 1) === false)
  }
}
