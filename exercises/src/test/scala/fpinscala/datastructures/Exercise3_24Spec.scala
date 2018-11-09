package fpinscala.datastructures

import org.scalatest._

class Exercise3_24Spec extends FlatSpec {

  "hasSubsequence(List(1, 2, 3), List(1))" should "return true" in {
    assert(List.hasSubsequence(List(1, 2, 3), List(1)))
  }

  "hasSubsequence(List(3), List(1))" should "return false" in {
    assert(!List.hasSubsequence(List(3), List(1)))
  }

  "list of strings" should "return true" in {
    assert(List.hasSubsequence(List("a", "b", "c", "hello"), List("a", "b")))
  }

  "false list of strings" should "return false" in {
    assert(!List.hasSubsequence(List("a", "b", "c", "hello"), List("a", "c")))
  }

  "list" should "return true" in {
    assert(List.hasSubsequence(List("a", "b", "c", "hello"), Nil))
  }
}
