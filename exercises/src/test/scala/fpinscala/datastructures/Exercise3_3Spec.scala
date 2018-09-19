package fpinscala.datastructures

import org.scalatest._

class Exercise3_3Spec extends FlatSpec {

  val testCases = Seq(
    (Nil, Nil, List(Nil)),
    (List(1, 2), Nil, List(Nil, 1, 2)),
    (Nil, 1, List(1)),
    (List(1), 2, List(2, 1))
  )

  for ((list, head, output) <- testCases) {
    s"List.setHead($list, $head)" should s"return $output" in {
      assert(List.setHead(list, head) === output)
    }
  }

}
