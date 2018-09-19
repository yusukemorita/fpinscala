package fpinscala.gettingstarted

import org.scalatest._
import fpinscala.datastructures._

class Exercise3_2Spec extends FlatSpec {

  "tail of List(1, 2, 3, 4)" should "return List(2, 3, 4)" in {
    val integerList = List(1, 2, 3, 4)
    assert(List.tail(integerList) === List(2, 3, 4))
  }

  "tail of List(hello, hi, goodbye)" should "return List(hi, goodbye)" in {
    val stringList = List("hello", "hi", "goodbye")
    assert(List.tail(stringList) === List("hi", "goodbye"))
  }

  "tail of List(2.1, 1.3)" should "return List(1.3)" in {
    val doubleList = List(2.1, 1.3)
    assert(List.tail(doubleList) === List(1.3))
  }

  "tail of list with one element" should "return Nil" in {
    val list = List(1)
    assert(List.tail(list) === Nil)
  }

  "tail of empty list" should "return Nil" in {
    assert(List.tail(Nil) === Nil)
  }

}
