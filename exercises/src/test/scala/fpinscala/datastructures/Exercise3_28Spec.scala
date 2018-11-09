package fpinscala.datastructures

import org.scalatest._

class Exercise3_28Spec extends FlatSpec {

  val square: Int => Int = (i: Int) => i * i

  val treesWithResults = Seq(
    (Leaf(1), Leaf(1), square),
    (Leaf(2), Leaf(4), square),
    (Leaf(3), Leaf(9), square)
  )

  for((tree, result, f) <- treesWithResults) {
    s"depth of ${tree.toString}" should s"return $result" in {
      assert(Tree.map(tree)(f) == result)
    }
  }
}
