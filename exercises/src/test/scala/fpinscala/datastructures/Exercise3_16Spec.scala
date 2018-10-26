package fpinscala.datastructures

import org.scalatest._

class Exercise3_16Spec extends FlatSpec {

  "addOneToEachElement(List(1)" should "return List(2)" in {
    assert(List.addOneToEachElement(List(1)) === List(2))
  }

  "addOneToEachElement(List(1, 2)" should "return List(2, 3)" in {
    assert(List.addOneToEachElement(List(1, 2)) === List(2, 3))
  }

  "addOneToEachElement(Nil)" should "return Nil" in {
    assert(List.addOneToEachElement(Nil) === Nil)
  }

  "addOneToEachElement(List(1, 2, 3, ... 1000))" should "return List(2, 3, ... 1001))" in {
    val list = (1 to 1000).foldLeft(Nil: List[Int])((acc, value) => List.setHead(acc, value))
    val resultList = (2 to 1001).foldLeft(Nil: List[Int])((acc, value) => List.setHead(acc, value))

    assert(List.addOneToEachElement(list) === resultList)
  }
}
