package fpinscala.laziness

import org.scalatest._

class Exercise5_1Spec extends FlatSpec {

  "stream(1).toList" should "return List(1))" in {
    val stream = Cons(() => 1, () => Empty)
    assert(stream.toListWithListBuffer === List(1))
  }

  "stream(1, 2).toList" should "return List(1, 2)" in {
    val stream = Cons(() => 1, () => Cons(() => 2, () => Empty))
    assert(stream.toListWithListBuffer === List(1, 2))
  }

  "Empty.toList" should "return Nil" in {
    assert(Empty.toListWithListBuffer === Nil)
  }
}
