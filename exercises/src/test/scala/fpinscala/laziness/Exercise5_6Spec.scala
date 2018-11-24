package fpinscala.laziness

import org.scalatest._

class Exercise5_6Spec extends FlatSpec {

  "stream(1).headOption" should "return Some(1)" in {
    val stream = Stream.cons(1, Empty)
    assert(stream.headOption === Some(1))
  }

  "Empty.headOption" should "return None" in {
    assert(Empty.headOption === None)
  }
}
