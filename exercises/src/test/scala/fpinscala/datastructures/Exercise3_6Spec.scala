package fpinscala.datastructures

import org.scalatest._

class Exercise3_6Spec extends FlatSpec {

  val intTestCases = Seq(
    (List(1, 2), List(1)),
    (List(1, 2, 3), List(1, 2))
  )

  for ((input, output) <- intTestCases) {
    s"init $input" should s"return $output" in {
      assert(List.init(input) === output)
    }
  }

}
