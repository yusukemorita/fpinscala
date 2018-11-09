package fpinscala.datastructures

import org.scalatest._

class Exercise3_26Spec extends FlatSpec {

  val leafA = Leaf(1)
  val leafB = Leaf(2)
  val branch = Branch(leafA, leafB)
  val branchWithBranch = Branch(Leaf(3), branch)

  val treesWithMaximums = Seq(
    (leafA, 1),
    (branch, 2),
    (branchWithBranch, 3)
  )

  for((tree, max) <- treesWithMaximums) {
    s"size of ${tree.toString}" should s"return $max" in {
      assert(Tree.maximum(tree) == max)
    }
  }
}
