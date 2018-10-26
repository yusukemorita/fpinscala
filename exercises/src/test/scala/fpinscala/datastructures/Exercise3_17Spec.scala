package fpinscala.datastructures

import org.scalatest._

class Exercise3_17Spec extends FlatSpec {

  "doublesInListToString(List(1.0)" should "return List('1.0')" in {
    assert(List.doublesInListToString(List(1.0)) === List("1.0"))
  }

  "doublesInListToString(List(1.0, 2.1)" should "return List('1.0', '2.1')" in {
    assert(List.doublesInListToString(List(1.0, 2.1)) === List("1.0", "2.1"))
  }
}
