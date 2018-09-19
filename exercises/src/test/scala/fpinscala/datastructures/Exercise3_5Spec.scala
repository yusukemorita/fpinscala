package fpinscala.datastructures

import org.scalatest._

class Exercise3_5Spec extends FlatSpec {

  val smallerThanThree: Int => Boolean = x => x < 3

  val testCases = Seq(
    (List(1, 2, 3, 4), smallerThanThree, List(3, 4)),
    (List(1, 2, 3, 4, 1, 2), smallerThanThree, List(3, 4, 1, 2)),
    (List(1, 2, -1, 0), smallerThanThree, Nil),
    (Nil, smallerThanThree, Nil)
  )

  for ((input, function, output) <- testCases) {
    s"dropwhile $input for $function" should s"return $output" in {
      assert(List.dropWhile(input, function) === output)
    }
  }
}
