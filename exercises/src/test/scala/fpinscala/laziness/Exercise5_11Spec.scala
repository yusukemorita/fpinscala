package fpinscala.laziness

import org.scalatest._

class Exercise5_11Spec extends FlatSpec {

  "Stream.unfold(0)((a: Int) => Some((a, a + 1))).take(5).toList" should "return List(0, 1, 2, 3, 4)" in {
    assert(Stream.unfold(0)((a: Int) => Some((a, a + 1))).take(5).toList === List(0, 1, 2, 3, 4))
  }

  "fibs.take(5).toList" should "return List(0, 1, 1, 2, 3)" in {
    assert(Stream.unfold((0, 1))((t: (Int, Int)) => Some(t._1, (t._2, t._1 + t._2))).take(5).toList === List(0, 1, 1, 2, 3))
  }

}
