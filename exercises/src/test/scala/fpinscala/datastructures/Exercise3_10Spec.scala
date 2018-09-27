package fpinscala.datastructures

import org.scalatest._

class Exercise3_10Spec extends FlatSpec {

  "foldLeft(List(1, 2), 0)((x, y) => x + y)" should "return 3" in {
    assert(List.foldLeft(List(1, 2, 3), 0)((x, y) => x + y) === 6)
  }

  "foldLeft(List(1, 2, 3), 0)((x, y) => x + y)" should "return 6" in {
    assert(List.foldLeft(List(1, 2, 3), 0)((x, y) => x + y) === 6)
  }

  "foldLeft(List(1, 2, 3), 1)((x, y) => x * y)" should "return 8" in {
    assert(List.foldLeft(List(1, 2, 4), 1)((x, y) => x * y) === 8)
  }

  "foldLeft(List(1, 2, 3), 0)((x, y) => x * y)" should "return 0" in {
    assert(List.foldLeft(List(1, 2, 4), 0)((x, y) => x * y) === 0)
  }
}
