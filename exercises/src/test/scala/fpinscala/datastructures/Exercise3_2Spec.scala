package fpinscala.datastructures

import org.scalatest._

class Exercise3_2Spec extends FlatSpec {

  val testCases = Seq(
    (List(1, 2, 3, 4), List(2, 3, 4)),
    (List("hello", "hi", "goodbye"), List("hi", "goodbye")),
    (List(2.1, 1.3), List(1.3)),
    (List(1), Nil),
    (Nil, Nil)
  )

  for ((input, output) <- testCases) {
    s"tail of $input" should s"return $output" in {
      assert(List.tail(input) === output)
    }
  }
}
